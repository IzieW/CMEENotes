### TensaFlow example from ML PDF

# simulate data with noise
exp <- replicate(10, rnorm(400))


resp <- exp[,1]*2 -0.5*exp[,2] - exp[,7]*exp[,8] + exp(abs(exp[,3]))
resp <- resp + rnorm(nrow(exp)) # add noise to the data!

exp <- as.matrix(scale(exp)); rep <- as.numeric(scale(resp))
training <- sample(nrow(exp), nrow(exp)/2)

# get keras ready- wrapper for TensaFlow
library(keras)
install_keras()

model <- keras_model_sequential() # initiate sequential model

# create model structure: 
model %>%
  layer_dense(units=15, activation="relu", input_shape=10) %>%
  layer_dense(units=15, activation="relu") %>%
  layer_dense(units=1)

# Compile model
model %>% compile(
  loss= "mean_squared_error",
  optimizer=optimizer_rmsprop(),
  metrics = c()
)

# train model with data
model %>% fit(exp[training,], resp[training], epoch=500)

plot(predict(model, exp[-training,],[,1]~resp[-training])
     cor.test(predict(model,exp[-training,])[,1],resp[-training])