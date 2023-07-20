library(tidyverse)


####### examples ########
# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

# Heatmap 
ggplot(data, aes(X, Y, fill= Z)) + 
            geom_tile()





dat <- matrix(rnorm(100, 3, 1), ncol = 10)
## the matrix needs names
names(dat) <- paste("X", 1:10)

## convert to tibble, add row identifier, and shape "long"
dat2 <-
            dat %>%
            as_tibble() %>%
            rownames_to_column("Var1") %>%
            pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
            mutate(
                        Var1 = factor(Var1, levels = 1:10),
                        Var2 = factor(gsub("V", "", Var2), levels = 1:10)
            )
#> Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if
#> `.name_repair` is omitted as of tibble 2.0.0.
#> ℹ Using compatibility `.name_repair`.

ggplot(dat2, aes(Var1, Var2)) +
            geom_tile(aes(fill = value)) +
            geom_text(aes(label = round(value, 1))) +
            scale_fill_gradient(low = "white", high = "red") +
            scale_x_discrete(position = "top") 


#### Mental health data

#Colours
#"#008080","#70a494","#b4c8a8","#f6edbd","#edbb8a","#de8a5a","#ca562c"


MH_data <- read.csv("data/model_selection_tables/adj_mod4_mh_both_ptable_mice_linear.csv")
