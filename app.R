library(shiny)
library(stringr)
library(dplyr)
library(ggplot2)
library(forcats)
library(httr2)
library(jqr)
library(jsonlite)
options(scipen = 7)

# Получение списка регионов
getRegionsList <- function() {
    if (!file.exists("./data")) {
        dir.create("./data")
    }
    regions_file_name <- paste0("./data/regions_", Sys.Date(), ".json")
    if (file.exists(regions_file_name)) {
        # Если есть сегодняшний файл, читаем из него
        regions <- read_json(regions_file_name, simplifyVector = TRUE)
    } else {
        # Если сегодняшнего файла нет, загружаем с сайта
        req <- request("https://api.hh.ru/areas")
        resp <- req %>% req_perform()
        if (resp_status(resp) == 200) {
            data <- resp_body_string(resp, encoding = "utf-8")
        } else {
            return(NULL)
        }

        regions <- data %>%
            # Выбираем список регионов России
            jq('[map(select(.name == "Россия")) | .[].areas[] | {name, id}]') %>%
            fromJSON()
        write_json(regions, regions_file_name)
    }
    # Преобразовываем в именованный список и сортируем
    regions_list <- as.list(setNames(regions$id, regions$name))
    regions_list["Россия"] <- "0"
    regions_list <- regions_list[order(as.integer(regions_list))]
    return(regions_list)
}

getCitiesList <- function() {
    if (!file.exists("./data")) {
        dir.create("./data")
    }
    cities_file_name <- paste0("./data/cities_", Sys.Date(), ".json")
    if (file.exists(cities_file_name)) {
        # Если есть сегодняшний файл, читаем из него
        cities <- read_json(cities_file_name, simplifyVector = TRUE)
    } else {
        # Если сегодняшнего файла нет, загружаем с сайта
        req <- request("https://api.hh.ru/areas")
        resp <- req %>% req_perform()
        if (resp_status(resp) == 200) {
            data <- resp_body_string(resp, encoding = "utf-8")
        } else {
            return(NULL)
        }

        cities <- data %>%
            # Выбираем список регионов России
            jq('[map(select(.name == "Россия")) | .[].areas[].areas[] | {id, region_id: .parent_id, name}]') %>%
            fromJSON()
        write_json(cities, cities_file_name)
    }
    # Преобразовываем в именованный список и сортируем
#    cities_list <- as.list(setNames(cities$id, cities$region_id, cities$name))
    # regions_list <- regions_list[order(as.integer(regions_list))]
    return(cities)
}


# Получение списка профессиональных ролей
getProfessionalRolesList <- function() {
    if (!file.exists("./data")) {
        dir.create("./data")
    }
    professional_roles_file_name <- paste0("./data/professional_roles_", Sys.Date(), ".json")
    if (file.exists(professional_roles_file_name)) {
        # Если есть сегодняшний файл, читаем из него
        professional_roles <- read_json(professional_roles_file_name, simplifyVector = TRUE)
    } else {
        # Если сегодняшнего файла нет, загружаем с сайта
        req <- request("https://api.hh.ru/professional_roles")
        resp <- req %>% req_perform()
        if (resp_status(resp) == 200) {
            data <- resp_body_string(resp, encoding = "utf-8")
        } else {
            return(NULL)
        }

        professional_roles <- data %>%
            # Выбираем список профессиональных ролей
            jq('[. | {categories}[] | map(select(.name == "Информационные технологии")) | .[].roles[] | {name, id}]') %>%
            fromJSON()
        write_json(professional_roles, professional_roles_file_name)
    }
    # Преобразовываем в именованный список
    return(as.list(setNames(professional_roles$id, professional_roles$name)))
}

regions_list <- getRegionsList()
professional_roles_list <- getProfessionalRolesList()
cities <- getCitiesList()

getRegionByCityName <- function(city_name) {
    res <- NULL
    for (cn in city_name) {
        if (length(cities[cities$name == cn, "region_id"]) > 0) {
            res <- res %>% append(cities[cities$name == cn, "region_id"])
        } else {
            res <- res %>% append(regions_list[cn])
        }
    }
    unlist(res)
}

getRegionByCityId <- function(city_id) {
    res <- NULL
    for (ci in city_id) {
        if (length(cities[cities$id == ci, "region_id"]) > 0) {
            res <- res %>% append(cities[cities$id == ci, "region_id"])
        } else {
            res <- res %>% append(ci)
        }
    }
    res
}

# Получение страницы с вакансиями
getVacanciesPage <- function(page = 0, professional_role) {
    req <- request("https://api.hh.ru/vacancies")
    req %>%
        req_body_form(page = page, professional_role = professional_role, area = 113, per_page = 100, only_with_salary = "true") %>%
        req_method("GET") %>%
        req_perform() -> resp
    if (resp_status(resp) == 200) {
        data <- resp_body_string(resp, encoding = "utf-8")
    } else {
       return(NULL)
    }
    return(data)
}

# Получение списка ссылок на вакансии
getVacanciesUrls <- function(professional_role) {
    urls_list <- vector()
    for (page in 0:19) {
        data <- getVacanciesPage(page, professional_role)
        pages_l <- data %>% 
            jq(". | {pages}") %>%
            fromJSON()
        pages <- pages_l$pages
        urls_list <- data %>%
            jq("[.items[] | {url}]") %>%
            fromJSON() %>%
            unlist() %>%
            c(urls_list)
        if (page == pages - 1) {
            break()
        }
        Sys.sleep(0.25)
    }
    return(urls_list)
}

# Получение вакансий
getVacancies <- function(professional_role){
    buck <- 82
    euro <- 90
    if (!file.exists("./data")) {
        dir.create("./data")
    }
    vacancies_file_name <- paste0("./data/vacancies_", as.character(professional_role), ".csv")
    skills_file_name <- paste0("./data/skills_", as.character(professional_role), ".csv")
    if (file.exists(vacancies_file_name) & file.exists(skills_file_name)) {
        vacancies <- read.csv(vacancies_file_name)
        skills <- read.csv(skills_file_name)
    } else {
        vacancies_urls <- getVacanciesUrls(professional_role)
        vacancies <- data.frame()
        skills <- data.frame()
        for (url in vacancies_urls) {
            req <- request(url)
            resp <- try(
                req %>%
                    req_perform()
            )
            if (inherits(resp, "try-error")) {
                break
            }
            if (resp_status(resp) == 200) {
                data <- resp_body_string(resp, encoding = "utf-8")
            }
            vacancy <- data %>%
                jq(". | {id, name, city_id: .area.id, city_name: .area.name, salary_from: .salary.from, salary_to: .salary.to, salary_currency: .salary.currency, salary_gross: .salary.gross, experience: .experience.name}") %>%
                fromJSON()
            # Если максимальная зарплата не указана, принимаем ее равной минимальной и наоборот
            vacancy$salary_to <- ifelse(is.null(vacancy$salary_to), vacancy$salary_from, vacancy$salary_to)
            vacancy$salary_from <- ifelse(is.null(vacancy$salary_from), vacancy$salary_to, vacancy$salary_from)
            if (length(vacancy) < 9 | any(sapply(vacancy, is.null))) {
                # Если не заполнены нужные нам поля, пропускаем вакансию
                next
            }
            if (length(vacancies) != 0) {
                vacancies <- rbind(vacancy, vacancies)
            } else {
                vacancies <- data.frame(vacancy)
            }
            cur_skills <- data %>%
                jq(". | {key_skills} | .[]") %>%
                fromJSON()
            if (length(cur_skills) != 0) {
                cur_skills$id <- vacancy$id
                if (length(skills) != 0) {
                    skills <- rbind(skills, cur_skills)
                } else {
                    skills <- cur_skills
                }
            }
            Sys.sleep(0.5)
        }
        if (nrow(vacancies) == 0) {
            return(NULL)
        }
        names(skills) <- c("skill", "vacancy")
        # Пересчитываем все зарплаты в сумму "на руки"
        vacancies$salary_from <- ifelse(vacancies$salary_gross, as.integer(vacancies$salary_from * 0.87), vacancies$salary_from)
        vacancies$salary_to <- ifelse(vacancies$salary_gross, as.integer(vacancies$salary_to * 0.87), vacancies$salary_to)
        # Пересчитываем долларовые и евровые зарплаты в рубли
        vacancies$salary_from <- ifelse(vacancies$salary_currency == "USD", vacancies$salary_from * buck, vacancies$salary_from)
        vacancies$salary_to <- ifelse(vacancies$salary_currency == "USD", vacancies$salary_to * buck, vacancies$salary_to)
        vacancies$salary_from <- ifelse(vacancies$salary_currency == "EUR", vacancies$salary_from * euro, vacancies$salary_from)
        vacancies$salary_to <- ifelse(vacancies$salary_currency == "EUR", vacancies$salary_to * euro, vacancies$salary_to)
        # Записываем полученные датафреймы в файлы
        write.csv(vacancies, file = vacancies_file_name, row.names = FALSE, quote = TRUE)
        write.csv(skills, file = skills_file_name, row.names = FALSE, quote = TRUE)
    }
    # Преобразуем опыт работы в упорядоченный фактор
    vacancies$experience <- factor(vacancies$experience, labels = c("Нет опыта", "От 1 года до 3 лет", "От 3 до 6 лет", "Более 6 лет"), ordered = TRUE)
    return(list(vacancies = vacancies, skills = skills))
}

# Получение названия профессии по id
getProfessionalRoleName <- function(pr) {
  names(professional_roles_list[professional_roles_list == pr])
}

# Получение из кэша сводного датафрейма по всем профессиям
getSummaryDf <- function() {
    res <- data.frame()
    for (pr in str_extract(list.files("./data/", "vacancies_\\d{2,3}.csv"), "\\d{2,3}")) {
        df <- getVacancies(pr)[["vacancies"]]
        df_row <- data.frame(professional_role = getProfessionalRoleName(pr),
                          salary = median((df$salary_from + df$salary_to) / 2),
                          vacancy_count = nrow(df))
        if (nrow(res) == 0) {
            res <- df_row
        } else {
            res <- rbind(res, df_row)
        }
    }
    res
}

ui <- fluidPage(
    # Заголовок приложения ----
    titlePanel("Анализ вакансий на hh.ru"),
    # Структура веб-страницы с главной и боковой панелями ----
    sidebarLayout(
        # Компоненты боковой панели для ввода исходных параметров ----
        sidebarPanel(
            # Input: Выбор региона ----
            selectInput(inputId = "region",
                        label = "Выбор региона",
                        choices = regions_list),
            # Input: Выбор профессии ----
            selectInput(inputId = "professional_role",
                        label = "Выбор профессии",
                        choices = professional_roles_list),
            # br() элемент включет вывод пустой строки ----
            br(),
            submitButton("Выполнить")
        ),
        mainPanel(
            h2(textOutput("text")),
            # Создание вкладок на главной панели
            tabsetPanel(type = "tabs",
                tabPanel("Востребованность",
                    h4(textOutput("count")),
                    # Output: График распределения ----
                    plotOutput("vacancy_skillsPlot"),
                    br(),
                    plotOutput("vacancy_experiencePlot")
                ),
                tabPanel("Зарплаты",
                    h4(textOutput("salary")),
                    plotOutput("salary_skillsPlot"),
                    br(),
                    plotOutput("salary_experiencyPlot"),
                    br(),
                    plotOutput("salary_experiency_boxPlot")
                ),
                tabPanel("Сравнение по регионам",
                    plotOutput("vacancies_cities"),
                    br(),
                    plotOutput("salary_cities")
                ),
                tabPanel("Сводные данные",
                    plotOutput("vacancies_summary"),
                    br(),
                    plotOutput("salary_summary")
                ),
                tabPanel("Данные",
                    dataTableOutput("dataset")
                )
            )
        )
    )
)

server <- function(input, output) {
    VacanciesSkills <- reactive({
        df <- getVacancies(input$professional_role)
        if (input$region != "0") {
            df$vacancies <- df$vacancies %>%
                filter(getRegionByCityId(df$vacancies$city_id) == input$region)
        }
        df
    })
    VacanciesSkillsAll <- reactive({
        getVacancies(input$professional_role)
    })
    output$vacancy_skillsPlot <- renderPlot({
        # some code
        dfVacanciesSkills <- VacanciesSkills()
        if (!is.null(dfVacanciesSkills)) {
            if (nrow(dfVacanciesSkills$vacancies) > 0) {
                dfVacanciesSkills$skills %>%
                    group_by(skill) %>%
                    summarise(vacancy_count = length(vacancy)) %>%
                    arrange(-vacancy_count) %>%
                    head(20) %>%
                    ggplot(aes(x = vacancy_count, y = fct_reorder(skill, vacancy_count))) +
                        geom_col(fill = "deepskyblue") +
                        xlab("Количество вакансий") +
                        ylab("Навык")
            }
        }
    })
    output$vacancy_experiencePlot <- renderPlot({
        dfVacanciesSkills <- VacanciesSkills()
        if (!is.null(dfVacanciesSkills)) {
            if (nrow(dfVacanciesSkills$vacancies) > 0) {
                dfVacanciesSkills$vacancies %>%
                    group_by(experience) %>%
                    summarise(vacancy_count = length(id)) %>%
                    arrange(-vacancy_count) %>%
                    ggplot(aes(x = experience, y = vacancy_count)) +
                        geom_col(fill = "deepskyblue") +
                        xlab("Опыт") +
                        ylab("Количество вакансий")
            }
        }
    })
    output$salary_skillsPlot <- renderPlot({
        # some code
        dfVacanciesSkills <- VacanciesSkills()
        if (!is.null(dfVacanciesSkills)) {
            if (nrow(dfVacanciesSkills$vacancies) > 0) {
                dfVacanciesSkills$skills %>% 
                    inner_join(dfVacanciesSkills$vacancies, by = join_by(vacancy == id)) %>%
                    group_by(skill) %>%
                    summarise(vacancy_count = length(vacancy), median_salary = median((salary_from + salary_to) / 2)) %>%
                    arrange(-median_salary) %>%
                    head(20) %>% 
                    ggplot(aes(x = median_salary, y = fct_reorder(skill, median_salary))) +
                        geom_col(fill = "deepskyblue") +
                        xlab("Медианная зарплата") +
                        ylab("Навык")
            }
        }
    })
    output$salary_experiencyPlot <- renderPlot({
        # some code
        dfVacanciesSkills <- VacanciesSkills()
        if (!is.null(dfVacanciesSkills)) {
            if (nrow(dfVacanciesSkills$vacancies) > 0) {
                dfVacanciesSkills$vacancies %>%
                    group_by(experience) %>%
                    summarise(vacancy_count = length(id), median_salary = median((salary_from + salary_to) / 2),
                                min_salary = min(salary_from), max_salary = max(salary_to)) %>%
                    ggplot(aes(x = experience)) +
                        geom_col(aes(y = min_salary), fill = "red",
                            width = 0.25,
                            position = position_nudge(x = -0.225)) +
                        geom_col(aes(y = median_salary), fill = "blue",
                                width = 0.25,
                                position = position_nudge(x = 0)) +
                        geom_col(aes(y = max_salary), fill = "green",
                                width = 0.25,
                                position = position_nudge(x = 0.225)) +
                        xlab("Опыт работы") +
                        ylab("Зарплата")
            }
        }
    })
    output$salary_experiency_boxPlot <- renderPlot({
        dfVacanciesSkills <- VacanciesSkills()
        if (!is.null(dfVacanciesSkills)) {
            if (nrow(dfVacanciesSkills$vacancies) > 0) {
                dfVacanciesSkills$vacancies %>%
                ggplot(aes(x = experience, y = (salary_from + salary_to) / 2)) +
                    geom_boxplot() +
                    xlab("Опыт работы") +
                    ylab("Медианная зарплата")
            }
        }
    })
    output$vacancies_cities <- renderPlot({
        dfVacanciesSkillsAll <- VacanciesSkillsAll()
        if (!is.null(dfVacanciesSkillsAll)) {
            if (nrow(dfVacanciesSkillsAll$vacancies) > 0) {
                dfVacanciesSkillsAll$vacancies %>% 
                    group_by(city_name) %>%
                    summarise(vacancy_count = length(id)) %>%
                    arrange(-vacancy_count) %>%
                    head(20) %>%
                    ggplot(aes(x = vacancy_count, y = fct_reorder(city_name, vacancy_count))) +
                        geom_col(fill = "deepskyblue") +
                        xlab("Количество вакансий") +
                        ylab("Город")
            }
        }
    })
    output$salary_cities <- renderPlot({
        dfVacanciesSkillsAll <- VacanciesSkillsAll()
        if (!is.null(dfVacanciesSkillsAll)) {
            if (nrow(dfVacanciesSkillsAll$vacancies) > 0) {
                dfVacanciesSkillsAll$vacancies %>% 
                    group_by(city_name) %>%
                    summarise(salary = median((salary_from + salary_to) / 2)) %>%
                    arrange(-salary) %>%
                    head(20) %>%
                    ggplot(aes(x = salary, y = fct_reorder(city_name, salary))) +
                        geom_col(fill = "deepskyblue") +
                        xlab("Медианная зарплата") +
                        ylab("Город")
            }
        }
    })
    output$vacancies_summary <- renderPlot({
        getSummaryDf() %>%
            ggplot(aes(x = vacancy_count, y = fct_reorder(professional_role, vacancy_count))) +
                geom_col(fill = "deepskyblue") +
                xlab("Количество вакансий") +
                ylab("Профессия")
    })
    output$salary_summary <- renderPlot({
        getSummaryDf() %>%
            ggplot(aes(x = salary, y = fct_reorder(professional_role, salary))) +
                geom_col(fill = "deepskyblue") +
                xlab("Медианная зарплата") +
                ylab("Профессия")
    })
    output$dataset <- renderDataTable(VacanciesSkills()[["vacancies"]])
    output$text <- renderText({
        paste("Анализ вакансий в регионе ", names(regions_list)[regions_list == input$region], 
              "по профессии", names(professional_roles_list)[professional_roles_list == input$professional_role])
    })
    output$count <- renderText({
        dfVacanciesSkills <- VacanciesSkills()
        if (!is.null(dfVacanciesSkills)) {
            if (nrow(dfVacanciesSkills$vacancies) > 0) {
                paste("Количество вакансий: ", toString(nrow(dfVacanciesSkills$vacancies)))
            }else {
                paste("Вакансии не найдены")
            }
        } else {
            paste("Вакансии не найдены")
        }
    })
    output$salary <- renderText({
        dfVacanciesSkills <- VacanciesSkills()
        if (!is.null(dfVacanciesSkills)) {
            if (nrow(dfVacanciesSkills$vacancies) > 0) {
                salary <- median((dfVacanciesSkills$vacancies$salary_from + dfVacanciesSkills$vacancies$salary_to) / 2)
                paste("Медианная зарплата: ", format(as.integer(salary), nsmall = 0, big.mark = " "), " рублей")
            } else {
                paste("Вакансии не найдены")
            }
        } else {
            paste("Вакансии не найдены")
        }
    })
}

shinyApp(ui, server)