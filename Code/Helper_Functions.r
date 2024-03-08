
libraries <- c("ggalluvial", "cowplot", "biscale", "sf", "rnaturalearth", 
               "rnaturalearthdata", "dplyr", "zoo", "scico", "ggplot2", 
               "ggpubr", "mapproj","lfe","jtools","stargazer","gsubfn","xml2","rvest","ggrepel")

install.packages(libraries)
lapply(libraries, library, character.only = TRUE)
## Helper Functions (start)        

    # reg_table_custom <- function(model_area1,title_table,dep.var.labels,name){
    #     P.adj.r.squared <-  summary(model_area1)$P.adj.r.squared
    #         P.r.squared <- summary(model_area1)$P.r.squared
            
    #         model_formula <- formula(model_area1)
    #         model_str <- as.character(model_formula)

    #         # Split the character string based on the pipe symbol
    #         parts <- unlist(strsplit(model_str, "\\|"))

    #         # The fixed effects are in the 2nd and potentially subsequent parts (excluding the last part, which specifies the instrumental variable component)
    #         fixed_effects <- unlist(strsplit(parts[2:(length(parts) - 1)], " + "))[3]
    #         fixed_effects_sep <- unlist(strsplit(fixed_effects, " \\+ "))

    #         # Create list for fixed effects
    #         fe_list <- lapply(fixed_effects_sep, function(x) c(paste0(x, " F.E."), "YES"))

    #         # Static lines
    #         static_lines <- list(c("-----------------------", "-----------------------"), 
    #                             c("R-squared (No F.E.)", format(P.r.squared, digits=3)),
    #                             c("Adjusted R-squared (No F.E.)", format(P.adj.r.squared, digits=3)))

    #         # Combine lists
    #         add.lines_all <- c(fe_list, static_lines)

        

    #         stargazer(model_area1,title=title_table, dep.var.labels=dep.var.labels, 
    #                                 type="html",
    #                                 #keep.stat="all",
    #                                 ci=TRUE,ci.level=0.90,single.row=TRUE,
    #                                 out = name, # omit the built-in R-squared to add manually
    #                                 add.lines = add.lines_all)

    # }

    # reg_table_custom_2 <- function(..., title_table, dep.var.labels, name){
  
    #     models <- list(...)
    #     n_models <- length(models)
        
    #     fe_list_all <- vector("list", n_models)
    #     P.adj.r.squared_all <- numeric(n_models)
    #     P.r.squared_all <- numeric(n_models)
        
    #     for(i in 1:n_models) {
            
    #         model <- models[[i]]
            
    #         P.adj.r.squared_all[i] <- summary(model)$P.adj.r.squared
    #         P.r.squared_all[i] <- summary(model)$P.r.squared
            
    #         model_formula <- formula(model)
    #         model_str <- as.character(model_formula)
            
    #         # Split the character string based on the pipe symbol
    #         parts <- unlist(strsplit(model_str, "\\|"))
            
    #         # The fixed effects are in the 2nd and potentially subsequent parts
    #         fixed_effects <- unlist(strsplit(parts[2:(length(parts) - 1)], " + "))[3]
    #         fixed_effects_sep <- unlist(strsplit(fixed_effects, " \\+ "))
            
    #         # Create list for fixed effects
    #         fe_list_all[[i]] <- lapply(fixed_effects_sep, function(x) c(paste0(x, " F.E."), "YES"))
            
    #     }
        
    #     # Static lines
    #     static_lines <- list(
    #                         c("R-squared (No F.E.)", format(P.r.squared_all, digits=3)),
    #                         c("Adjusted R-squared (No F.E.)", format(P.adj.r.squared_all, digits=3)))
        
    #     # Combine lists
    #     add.lines_all <- c(fe_list_all, list(static_lines))
        
    #     stargazer(..., title=title_table, dep.var.labels=rep(dep.var.labels, n_models), 
    #                 type="html",
    #                 #keep.stat="all",
    #                 ci=FALSE,# ci.level=0.90, 
    #                 single.row=TRUE,
    #                 out = name, 
    #                 se = NULL,
    #                 report = "vc*", 
    #                 add.lines = unlist(add.lines_all, recursive = FALSE))
    # }

    replace_numbers = function(x, cutoff=4, digits=3, scipen=-2) {
        ifelse(nchar(x) < cutoff, x, prettyNum(as.numeric(x), digits=digits, scientific=scipen))
    }

    reg_table_custom <- function(..., title_table, dep.var.labels, name,sci,replacements) {
  
        models <- list(...)
        n_models <- length(models)
        
        fe_list_all <- vector("list", n_models)
        P.adj.r.squared_all <- numeric(n_models)
        P.r.squared_all <- numeric(n_models)
        area_weighted <- rep("NO", n_models)  # Default to "NO"
        gridcell_fe <- rep("NO", n_models)  # Default to "NO"
        year_fe <- rep("NO", n_models)  # Default to "NO"
        timetrend <- rep("NO", n_models)  # Default to "NO"
        clust<- rep("NO", n_models)  # Default to "NO"
        
        for(i in 1:n_models) {
    
            model <- models[[i]]
            
            P.adj.r.squared_all[i] <- summary(model)$P.adj.r.squared
            P.r.squared_all[i] <- summary(model)$P.r.squared
            
            # Check for weighted regression
            if(any(!is.null(model$weights) && !all(model$weights == 1))) {
            area_weighted[i] <- "YES"
            } else {
            area_weighted[i] <- "NO"
            }
            
            model_formula <- formula(model)
            model_str <- as.character(model_formula)
            
            # Check if "gridcell_id" or "year" is in the formula as a fixed effect
            if (any(grepl("gridcell_id", model_str))) {
                gridcell_fe[i] <- "YES"
            } else {
                gridcell_fe[i] <- "NO"
            }
                
            if (any(grepl("year", model_str))) {
                year_fe[i] <- "YES"
            } else {
                year_fe[i] <- "NO"
            }

            if (any(grepl("year:countrycode", model_str))) {
                timetrend[i] <- "Linear"
            } else {
                timetrend[i] <- "NO"
            }

            if (any(grepl("I(year^2):countrycode", model_str))) {
                timetrend[i] <- "Quadratic"}

            if (any(grepl("0 | gridcell_id", model_str))) {
                clust[i] <- "YES"}

            
            # Split the character string based on the pipe symbol
            #parts <- unlist(strsplit(model_str, "\\|"))
            
            # The fixed effects are in the 2nd and potentially subsequent parts
            #fixed_effects <- unlist(strsplit(parts[2:(length(parts) - 1)], " + "))[3]
            #fixed_effects_sep <- unlist(strsplit(fixed_effects, " \\+ "))
            
            # Create list for fixed effects
            #fe_list_all[[i]] <- lapply(fixed_effects_sep, function(x) c(paste0(x, " F.E."), "YES"))
            
        }


        
        # Static lines
        static_lines <- list(c("Country Time Trend", timetrend),
                            c("Gridcell F.E.", gridcell_fe),
                            c("Year F.E.", year_fe),
                            c("Area Weighted", area_weighted),
                            c("Gridcell-level Clustered S.E.", clust),
                            c("R-squared (No F.E.)", format(P.r.squared_all, digits=3)),
                            c("Adjusted R-squared (No F.E.)", format(P.adj.r.squared_all, digits=3)))
        
        # Combine lists
        #add.lines_all <- c(fe_list_all, list(static_lines))
        add.lines_all <- c(list(static_lines))
        
        # stargazer(..., title=title_table, dep.var.labels=rep(dep.var.labels, n_models), 
        #             type="html",
        #             single.row=TRUE,
        #             out = name, 
        #             se = NULL,
        #             report = "vc*", 
        #             add.lines = unlist(add.lines_all, recursive = FALSE))

        # Regular table
        if(sci==TRUE){
            digits_sci = 8
            base <- sub("\\.html$", "", name) 
            extension <- "html"
            rep_s = "vc*s"
            name <- paste0(base, "_sci.", extension)
            single_row = TRUE
        } else{ digits_sci=3
                rep_s = "vc*"}
                single_row = FALSE

        stargazer(..., title=title_table, dep.var.labels=rep(dep.var.labels, n_models), 
                type="html",
                single.row=single_row,
                digits=digits_sci,
                out = name, 
                se = NULL,
                report = rep_s, 
                add.lines = unlist(add.lines_all, recursive = FALSE))
        

  
  # Read the HTML file into a string
  html_content <- readLines(name, warn = FALSE)
  
  # Loop through the replacements and make each change
  for (old_str in names(replacements)) {
    new_str <- replacements[[old_str]]
    html_content <- gsub(old_str, new_str, html_content, fixed = TRUE)
  }


  
    # Convert the modified string to an XML document object
    doc <- read_html(paste0(html_content, collapse = "\n"))
  
    # Identify rows that come right after rows where the first column contains "year:" and remove those rows
    rows_to_remove_next <- html_nodes(doc, xpath = "//tr[td[1][contains(., 'year:')]]/following-sibling::tr[1]")

    

    # Identify rows where first column contains "year" and remove those rows
    rows_to_remove <- html_nodes(doc, xpath = "//tr[td[1][contains(., 'year:')]]")
    xml_remove(rows_to_remove_next)
    xml_remove(rows_to_remove)
      # Find rows that have only whitespace (or are empty) and remove them
    #empty_rows_without_style <- html_nodes(doc, xpath = "//tr[not(normalize-space(.)) and not(@style)]") 
    #xml_remove(empty_rows_without_style)
    # Find rows that have only whitespace (or are empty) and do not have a style attribute, then remove them
    # Find rows that have only whitespace (or are empty) and none of its child <td> elements have a style attribute, then remove them
    empty_rows <- html_nodes(doc, xpath = "//tr[not(td[normalize-space(.)])][position() > 4]")
    xml_remove(empty_rows)




  

  #html_nodes(doc, "table") %>% html_attr("style", "border-collapse: collapse;")


  # Identify rows where the first column contains "Gridcell F.E."
  gridcell_rows <- html_nodes(doc, xpath = "//tr[td[1][contains(., 'Gridcell F.E.')]]")

  # For each of these rows, fill the remaining cells with "YES"
  # For each of these rows, fill the remaining cells with "YES"
    for (row in gridcell_rows) {
    cells <- xml_children(row)
    for (i in 2:length(cells)) {
        xml_text(cells[[i]]) <- "YES"
    }
    }

    gridcell_rows <- html_nodes(doc, xpath = "//tr[td[1][contains(., 'Year F.E.')]]")

  # For each of these rows, fill the remaining cells with "YES"
  # For each of these rows, fill the remaining cells with "YES"
    for (row in gridcell_rows) {
    cells <- xml_children(row)
    for (i in 2:length(cells)) {
        xml_text(cells[[i]]) <- "YES"
    }
    }


    r2_rows <- html_nodes(doc, xpath = "//tr[td[contains(., 'R')]]")

    # For each of these rows, style the top border of each cell
    for (row in r2_rows[1]) {
      ncol <- length(xml_children(row))
    new_row <- read_xml(sprintf('<tr><td colspan="%s" style="border-bottom: 1px solid black;"></td></tr>', ncol))
    xml_add_sibling(row, new_row, .where = "before")
  }

  
  country_rows <- html_nodes(doc, xpath = "//tr[td[contains(., 'Country Time')]]")

    # For each of these rows, style the top border of each cell
    for (row in country_rows[1]) {
      ncol <- length(xml_children(row))
    new_row <- read_xml(sprintf('<tr><td colspan="%s" style="border-bottom: 1px solid black;"></td></tr>', ncol))
    xml_add_sibling(row, new_row, .where = "before")
  }
  note_rows <- html_nodes(doc, xpath = "//tr[td[contains(., 'Note')]]")

    # For each of these rows, style the top border of each cell
    for (row in note_rows[1]) {
      
    new_row <- read_xml(sprintf('<tr><td colspan="%s" style="border-bottom: 1px solid black;"></td></tr>',ncol))
    xml_add_sibling(row, new_row, .where = "before")
  }
  
  # Convert the XML document object back to a string
  modified_html_content <- as.character(doc)
  if(sci==TRUE){
  modified_html_content <- gsubfn("([0-9.]+)", ~replace_numbers(x), modified_html_content)}
  # Save the modified string to an HTML file
  
    writeLines(modified_html_content, con = name)
  
    writeLines(modified_html_content, con = name)
  
}



# Example usage:
replacements_list <- list(
    "I("="",
    "2)"="<sup>2</sup>",
    "(<sup>2</sup>"="(2)",
    "sst_hottest"="SST of the Hottest Month",
  "preci" = "Precipitation",
  "sst" = "SST",
  "gridcell_id" = "Gridcell",
  "R-squared"="R<sup>2</sup>",
  "logGDPpc_country"="Country-level GDP per capita",
  "logGDPpc"="Log GDP per capita",
  "(lag_gap_density)"="lag_gap_density",
  "gridcellF.E."="Gridcell F.E.",
  "logPop"="Log Population",
  "year F.E."="Year F.E.",
  "lag_gap_density"="Lag Gap Density",
  "hot_bin"="Days Above Threshold",
  "mcw_int_anom_neg:mcw_freq"="MCW intxfreq",
  "mhw_int_anom:mhw_freq"="MHW intxfreq"
)

# Replace strings in the same file


# OR if you want to save changes to a new file
# replace_strings_in_html("path_to_html_file.html", replacements_list, save_as_same_file = FALSE, new_file_path = "new_html_file.html")




# Use
# reg_table_custom(model1, model2, model3, title_table="Your Title", dep.var.labels="Your Label", name="output.html")


    
    sqest <- function(data, model, namevar, exp) {
            dataset <- data
            Sigma <- vcov(model)
            coefT <- namevar
            start1 <- which(names(coef(model))==coefT)
            end1 <- which(names(coef(model))==paste("I(",coefT,"^2)",sep=""))
            
            sigma = Sigma[c(start1:end1),c(start1:end1)]
            beta.hat <- coef(model)[c(start1:end1)]
            # Calculate the sum of the coefficients
            sum_coef = sum(beta.hat)
            
            # Compute the standard error of the sum
            sum_se = sqrt(sum(sigma))
            
            # Calculate the t-statistic
            t_stat = sum_coef / sum_se
            
            # Compute the p-value
            p_value = 2 * (1 - pt(abs(t_stat), df = df.residual(model)))


            x <- seq(from=min(dataset[,which(names(dataset)==namevar)],na.rm=TRUE),to=max(dataset[,which(names(dataset)==namevar)],na.rm=TRUE), length=100)
            xmat <- cbind(1, 2*x)
            gestimated <- colSums(beta.hat*t(xmat)) 
            ci12 <- gestimated + 1.64*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
            ci22 <- gestimated -  1.64*sqrt(diag((xmat %*% sigma) %*% t(xmat)))
            significant <- rep("Not significant (p > 0.05)",length(x))
            ret_df <- data.frame(gestimated=gestimated,ci1=ci12,ci2=ci22,exp=exp,temp=x, p_value=p_value,significant=significant,sum_se =sum_se)
            ret_df$significant[which(ret_df$p_value<0.05)] <- "Significant (p < 0.05)"
            return(ret_df)
        }

    model_diagnostics <- function(model, data, variable,filename) {
        
            # Extract residuals and fitted values
            residuals <- resid(model)
            fitted <- fitted.values(model)
            
            df_resid <- data.frame(Residuals = residuals, Fitted = fitted)
            names(df_resid) <- c("Residuals" , "Fitted")
            
            df_resid$Year <-  data$year
            df_resid$Actual <- variable
            
            glimpse(df_resid)

            # 1. Residuals vs Fitted
            p1 <- ggplot(df_resid, aes(x = Fitted, y = Residuals)) + 
                geom_point() + 
                geom_smooth(se = FALSE, method = "loess") + 
                theme_minimal() +
                ggtitle("Residuals vs Fitted")
            
            # 2. Check for Normality of Residuals
            p2 <- ggplot(df_resid, aes(sample = Residuals)) +
                stat_qq() +
                geom_qq_line() +
                theme_minimal() +
                ggtitle("QQ Plot of Residuals")

            # 3. Residuals over Time 
            p3 <- ggplot(df_resid, aes(x = Year, y = Residuals)) + 
                geom_point() + 
                geom_smooth(se = FALSE, method = "loess") + 
                theme_minimal() +
                ggtitle("Residuals over Time")

            # 4. Actual vs Predicted
            p4 <- ggplot(df_resid, aes(x = Actual, y = Fitted)) + 
                geom_point() + 
                geom_smooth(method = "lm", color = "red") + 
                theme_minimal() +
                ggtitle("Actual vs Predicted")

            # Arrange the plots in a 2x2 grid
            grid.arrange(p1, p2, p3, p4, ncol = 2)

            

    }


    
    long_dif <- function(data, variable_names) {
            if(length(variable_names) < 1) stop("Please provide at least one variable name.")
            
            # Create renaming vector for 2015
            #rename_2015 <- setNames(paste0(variable_names, "_2015"), variable_names)
            rename_2015 <- setNames(variable_names,paste0(variable_names, "_2015"))
            
            # Perform the data manipulation
            result <- data %>%
                filter(year %in% c(2015, 2020)) %>%
                # Self join to match 2015 and 2020 for each gridcell_id
                inner_join(data %>% filter(year == 2015) %>% 
                            select(c("gridcell_id", variable_names)) %>%
                            rename(!!!rename_2015), 
                        by = "gridcell_id") %>%
                filter(year == 2020) %>%
                # Create mutations for percent change for all variables
                mutate(across(all_of(variable_names), 
                            ~ (.x - get(paste0(cur_column(), "_2015"))) / get(paste0(cur_column(), "_2015")) * 100, 
                            .names = "change_{.col}")) #%>%select(gridcell_id,countrycode,temp, starts_with("change_"))
            
            return(result)
        }


        long_dif_delta <- function(data, variable_names) {
            if(length(variable_names) < 1) stop("Please provide at least one variable name.")
            
            # Create renaming vector for 2015
            #rename_2015 <- setNames(paste0(variable_names, "_2015"), variable_names)
            rename_2015 <- setNames(variable_names,paste0(variable_names, "_2015"))
            
            # Perform the data manipulation
            result <- data %>%
                filter(year %in% c(2015, 2020)) %>%
                # Self join to match 2015 and 2020 for each gridcell_id
                inner_join(data %>% filter(year == 2015) %>% 
                            select(c("gridcell_id", variable_names)) %>%
                            rename(!!!rename_2015), 
                        by = "gridcell_id") %>%
                filter(year == 2020) %>%
                # Create mutations for percent change for all variables
                mutate(across(all_of(variable_names), 
                            ~ (.x - get(paste0(cur_column(), "_2015"))), 
                            .names = "change_{.col}")) #%>%select(gridcell_id,countrycode,temp, starts_with("change_"))
            
            return(result)
        }
        
        
        adjust_and_plot <- function(model, data, variable,filename,save) {
  
            # Get list of unique fixed effects
            fe_list <- unique(getfe(model)$fe)
            
            # Extract and merge all FEs to data
            for(fe in fe_list) {
                fe_df <- getfe(model) %>% dplyr::filter(fe == !!fe)
                names(fe_df)[which(names(fe_df) == "idx")] <- fe
                names(fe_df)[which(names(fe_df) == "effect")] <- paste0("effect_",fe)
                data <- merge(data, fe_df, by = fe)
            }

            data$predicted <- as.numeric(fitted(model))

            data <- data %>%
                mutate(adjusted_actual = variable  - rowSums(select(., starts_with("effect_")), na.rm = TRUE),
                    adjusted_predicted = predicted - rowSums(select(., starts_with("effect_")), na.rm = TRUE),
                    dif_pred = adjusted_predicted - adjusted_actual)
            
            data$selected_effect <- unlist(data %>% select(paste0("effect_",fe_list[1])))
            glimpse(data)
            
            # Plot 1
            p1 <- ggplot(data) +
                geom_point(aes(x = logGDPpc_country, y = selected_effect , col = temp)) +
                ylab(paste0(fe_list[1]," Effect")) +
                theme_bw() +
                scale_color_scico() +
                geom_smooth(aes(x = logGDPpc_country, y = selected_effect ), formula = y ~ x + I(x^2))
            
            # Plot 2
            p2 <- ggplot(data, aes(x = adjusted_predicted, y = adjusted_actual, color = year)) + 
                geom_point(aes(color = countrycode)) + 
                geom_smooth(method = "lm", color = "red") + 
                xlab("predicted")+ylab("actual")+
                theme_minimal() +
                ggtitle("Actual vs Predicted")+guides(color="none")

            # Plot 2
            p3 <- ggplot(data, aes(x = adjusted_predicted, y = adjusted_actual, color = year)) + 
                geom_line(aes(group = gridcell_id)) + 
                geom_smooth(method = "lm", color = "red") + 
                xlab("predicted")+ylab("actual")+
                theme_minimal() +
                ggtitle("Actual vs Predicted (by gridcell)")
            
            # Plot 3
            p4 <- ggplot(data, aes(x = adjusted_predicted, y = dif_pred, color = year)) + 
                geom_point(aes(color = countrycode)) + 
                geom_smooth(method = "lm", color = "red") + 
                theme_minimal() +
                ggtitle("Residuals vs  Predicted")+guides(color="none")+xlab("predicted")
            
            # Return plots (this will only print the last one, you'll need to explicitly print others if needed)

            coefs_pref_holes <- plot_coefs(model, ci_level = 0.90)
            
            coefs_pref_holes
            
            #grid.arrange(p1, p2, p3, p4, ncol = 2)


            # 2. Check for Normality of Residuals
            residuals <- resid(model)
            fitted <- fitted.values(model)
            
            df_resid <- data.frame(Residuals = residuals, Fitted = fitted)
            names(df_resid) <- c("Residuals" , "Fitted")
            
            p_qq <- ggplot(df_resid, aes(sample = Residuals)) +
                stat_qq() +
                geom_qq_line() +
                theme_minimal() +
                ggtitle("QQ Plot of Residuals")+
                xlab("Sample Quantiles (Standardized Residuals)") +
                ylab("Theoretical Quantiles")

            
              # Retrieve the formula from the felm model
            model_formula <- formula(model)

            # Convert the formula into a character string for display
            formula_text <- as.character(paste(model_formula[2],model_formula[1],model_formula[3]))

            # Create the title using textGrob
            formula_title <- grid::textGrob(formula_text, gp = grid::gpar(fontsize = 7))

            model_summary <- summary(model)

            # Retrieve the adjusted R^2
            adjusted_r2 <- model_summary$P.adj.r.squared
            adjusted_r2_text <- sprintf("Adjusted R^2 (Proj): %.6f", adjusted_r2)


            # Create the footnote using textGrob
            r2_footnote <- grid::textGrob(adjusted_r2_text, gp = grid::gpar(fontsize = 8))

            # Include the footnote in the arrangeGrob function
            combined_plot <- gridExtra::arrangeGrob(coefs_pref_holes, p1, p2, p3, p_qq, p4, ncol = 2, top = formula_title, bottom = r2_footnote)
            
            if(save==TRUE){ggsave(filename, combined_plot)}
            # Arrange the plots with the formula as a top title
            gridExtra::grid.arrange(coefs_pref_holes, p1, p2, p3, p_qq, p4, ncol = 2, top = formula_title, bottom = r2_footnote)

            

    }



    adjust_and_plot_long <- function(model, data, variable, filename, save) {

        data$predicted <- as.numeric(fitted(model))

        # No need to adjust for fixed effects, just get the difference between predicted and actual
        data$dif_pred = data$predicted - variable

        # Plot 1: Actual vs. Predicted
        p1 <- ggplot(data, aes(x = predicted, y = variable, color = year)) + 
            geom_point(aes(color = countrycode)) + 
            geom_smooth(method = "lm", color = "red") + 
            xlab("predicted")+ylab("actual")+
            theme_minimal() +
            ggtitle("Actual vs Predicted")+guides(color="none")

       

        # Plot 3: Residuals vs. Predicted
        p3 <- ggplot(data, aes(x = predicted, y = dif_pred, color = year)) + 
            geom_point(aes(color = countrycode)) + 
            geom_smooth(method = "lm", color = "red") + 
            theme_minimal() +
            ggtitle("Residuals vs Predicted")+guides(color="none")+xlab("predicted")

        # Check for Normality of Residuals
        residuals <- resid(model)
        p_qq <- ggplot(data.frame(residuals = residuals), aes(sample = residuals)) +
            stat_qq() +
            geom_qq_line() +
            theme_minimal() +
            ggtitle("QQ Plot of Residuals")+
            ylab("Sample Quantiles (Standardized Residuals)") +
            xlab("Theoretical Quantiles")

        model_formula <- formula(model)
        formula_text <- as.character(paste(model_formula[2],model_formula[1],model_formula[3]))
        formula_title <- grid::textGrob(formula_text, gp = grid::gpar(fontsize = 7))
        
        model_summary <- summary(model)
        adjusted_r2 <- model_summary$adj.r.squared
        adjusted_r2_text <- sprintf("Adjusted R^2: %.6f", adjusted_r2)
        r2_footnote <- grid::textGrob(adjusted_r2_text, gp = grid::gpar(fontsize = 8))

        coefs_pref_holes <- plot_coefs(model, ci_level = 0.90)
            
            

        combined_plot <- gridExtra::arrangeGrob(coefs_pref_holes, p1, p3, p_qq, ncol = 2, top = formula_title, bottom = r2_footnote)

        if (save == TRUE) {
            ggsave(filename, combined_plot)
        }

        # Display the combined plots
        gridExtra::grid.arrange(coefs_pref_holes,p1,  p3, p_qq, ncol = 2, top = formula_title, bottom = r2_footnote)
    }



## Helper Functions (start)
    