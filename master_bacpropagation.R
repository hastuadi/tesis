install.packages('neuralnet')
library("neuralnet")

#Going to create a neural network to perform sqare rooting
#Type ?neuralnet for more information on the neuralnet library

#Generate 50 random numbers uniformly distributed between 0 and 100
#And store them as a dataframe
traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=10, threshold=0.01)
print(net.sqrt)

#Plot the neural network
plot(net.sqrt)

#Test the neural network on some training data
testdata <- as.data.frame((1:10)^2) #Generate some squared numbers
net.results <- compute(net.sqrt, testdata) #Run them through the neural network

#Lets see what properties net.sqrt has
ls(net.results)

#Lets see the results
print(net.results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)

#xor examples
x.1 <- c(0,1,0,1)
x.2 <- c(0,0,1,1)
x.in <- cbind(x.1,x.2)
x.out <- c(0,1,1,0)
x.training <- cbind(x.in,x.out)
x.training <- as.data.frame(x.training)
x.training
nn.res <- neuralnet(formula = x.out ~ x.1 + x.2,
                    data = x.training, hidden = 2, threshold = 0.001)
print(nn.res)
test.result <- compute(nn.res,x.training[,c('x.1','x.2')])
test.result$net.result

#----test 3 input variables, two output variables
library(neuralnet)
xin <- expand.grid(c(0,1),c(0,1),c(0,1))
xin <- xin[,rev(c(1:3))]
xout.1 <- c(rep(10,4),rep(20,4))
xout.2 <- rep(rep(c(0,1),each = 2),times = 2)
color.3 <- rep(2:5,each = 2)
x.train.3 <- as.data.frame(cbind(xin,xout.1,xout.2))
nn.res.3 <- neuralnet(
   formula = xout.1 + xout.2 ~ Var3 + Var2 + Var1,
   data = x.train.3, hidden = 5, threshold = .001
)
print(nn.res.3)
test.result.3 <- compute(nn.res.3,x.train.3[,1:3])
test.result.3$net.result
