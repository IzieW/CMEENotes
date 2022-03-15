# CNN Practical 

# Load data
mnist <- dataset_mnist() #list of data

x_train <- mnist$train$x
y_train <- mnist$train$y

x_test <- mnist$test$x
y_test <- mnist$test$y



# a. train a flat deep learning network with at least two hidden layers
# These are handwritten digests, 0-9, so set up your output layer accordingly.

# plot quickly
lookup <- seq(1, 9, 1)
x_train_n <- x_train/255
par(mfrow=c(5,5))
for(i in seq_len(5*5)){
  image(t(x_train_n[i, 28:1,]), main=lookup[y_train[i]+1], col=grey.colors(255
                                                                            ))
}
# dealing with hand writting images 0-9

# define model 
model <- keras_model_sequential()
model %>%
  layer_flatten(input_shape=c(28,28)) %>%
  layer_dense(units=128, activation = "relu") %>%
  layer_dropout(rate=0.5) %>% 
  layer_dense(units=10, activation="softmax")

# compile model 
model %>% compile(
  optimizer="adam",
  loss= "sparse_categorical_crossentropy",
  metrics=c("accuracy")
)

# train model
model %>% fit(x_train, y_train, epoch=5)

# test
model %>% evaluate(x_test, y_test)
