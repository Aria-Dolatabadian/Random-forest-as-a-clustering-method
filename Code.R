library(tidymodels)
set.seed(123)
data<- iris
head(data)


table(data$Species)

data_split <- initial_split(data, strata = "Species")
data_train <- training(data_split)
data_test <- testing(data_split)

rf_recipe <- 
  recipe(formula = Species ~ ., data = data_train) %>%
  step_zv(all_predictors())

## feature importance sore to TRUE, and the proximity matrix to TRUE
rf_spec <- rand_forest() %>%
  set_engine("randomForest", importance = TRUE, proximity = TRUE) %>%
  set_mode("classification")

rf_workflow <- workflow() %>% 
  add_recipe(rf_recipe) %>% 
  add_model(rf_spec)


rf_fit <- fit(rf_workflow, data = data_train)

## confusion matrix
predict(rf_fit, new_data = data_test) %>%
        bind_cols(data_test %>% select(Species)) %>%
        conf_mat(truth = Species, estimate = .pred_class)

proximity_mat<- rf_fit$fit$fit$fit$proximity

pca_prcomp<- prcomp(proximity_mat, center = TRUE, scale. = FALSE)
plot(pca_prcomp)



pca_df<- data.frame(pca_prcomp$x[,1:2], Species = data_train$Species)

ggplot(pca_df, aes(x= PC1, y = PC2)) +
        geom_point(aes(color = Species))

head(data_train)


pca_prcomp2<- prcomp(as.matrix(data_train[, -5]), center = TRUE, scale. = FALSE)

plot(pca_prcomp2)

pca_df2<- data.frame(pca_prcomp2$x[,1:2], Species = data_train$Species)

ggplot(pca_df2, aes(x= PC1, y = PC2)) +
        geom_point(aes(color = Species))


dim(proximity_mat)

proximity_mat[1:5, 1:5]

rownames(proximity_mat)<- data_train[, 5]
colnames(proximity_mat)<- data_train[, 5]
# turn it to a distance 
iris_distance<- dist(1-proximity_mat)

# hierarchical clustering
hc<- hclust(iris_distance)

# cut the tree to 3 clusters
clusters<- cutree(hc, k = 3)


library(dendextend)
library(dendsort)
library(Polychrome)

mypal <- kelly.colors(4)[-1]
swatch(mypal)


plot_dend<- function(dend,...){
  dend_labels<- dend %>% labels()
  
  dend %>% 
  color_labels(col = mypal[as.numeric(as.factor(dend_labels))]) %>%
  set("labels_cex", 0.7) %>%
  plot(...)
}

# no sorting on dendrogram
as.dendrogram(hc) %>%
        plot_dend()

# sort the dendrogram
as.dendrogram(hc) %>%
        dendsort() %>%
        plot_dend()


pca_df<- data.frame(pca_prcomp$x[,1:2], 
                    Species = data_train$Species,
                    clusters = as.factor(clusters))

ggplot(pca_df, aes(x= PC1, y = PC2)) +
        geom_point(aes(color = Species, shape = clusters)) +
        theme_bw(base_size = 14)


pca_df2<- data.frame(pca_prcomp2$x[,1:2], 
                    Species = data_train$Species,
                    clusters = as.factor(clusters))

ggplot(pca_df2, aes(x= PC1, y = PC2)) +
        geom_point(aes(color = Species, shape = clusters)) +
        theme_bw(base_size = 14)










