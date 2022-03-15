# Convolutional Neural Networks: Basic Image Identification

library(keras)
install_keras()

# Load data- built in to keras
raw_data <- dataset_fashion_mnist()
resp <- raw_data$train$y
exp <- raw_data$train$x

# Do a bit of renaming and scaling and plot for sense. 
lookup <- c("Tshirt/top", "Trousders", "Pullover", "Dress",
           "Coat", "Sandal", "Shirt", "Sneaker", "Bag",
           "Ankle boot")

exp <- exp/225 # divide by its maximum number- scale down

par(mfrow=c(5,5))

for(i in seq_len(5*5)){
  image(t(exp[i, 28:1,]), main=lookup[resp[i]+1], col=grey.colors(225))
} # plots all the clothing items

## Define the model
model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(28, 28)) %>% # flattens images into 1D shapes (vectors)
  layer_dense(units=128, activation="relu") %>% # remove 0 space
  layer_dropout(rate=0.5) %>%
  layer_dense(units=10, activation="softmax")

# Compile model
model %>% compile(
  optimizer="adam",
  loss = "sparse_categorical_crossentropy",
  metrics=c("accuracy")
)

# Fit model and indepently validate
model %>% fit(exp, resp, epochs=5)
test.resp <- raw_data$test$y
test.exp <- (raw_data$test$x)/255

model %>% evalusate(test.exp, test.resp)
predictions <- model %>% predict(test.exp)
table(apply(predictions, 1, which.max)-1, test.resp)

##### FITTING A CNN ####

# model specification
conv <- keras_model_sequential()
conv %>%
  layer_conv_2d(filters=20, kernel_size=c(3,3), activation="relu", 
                input_shape=c(28,28,1)) %>% 
  layer_max_pooling_2d(pool_size=c(2,2)) %>%
  layer_flatten() %>%
  layer_dense(units=20, activation='relu') %>%
layer_dense(units=10, activation="softmax") %>%
  compile(
    optimizer="adam",
    loss= "sparse_categorical_crossentropy",
    metrics=c("accuracy")
  )

# re-arrange data and fit model

array.exp <- array(exp, dim=c(dim(exp), 1)) # add extra dimension
conv %>% fit(array.exp, resp, epochs=10)