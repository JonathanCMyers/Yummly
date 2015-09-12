#install.packages("rjson")
library(rjson)
#setwd("E:/Yummly")
document <- fromJSON(file="train.json")
#print(1)

cuisines <- c("greek", "southern_us", "filipino", "indian", "jamaican", "spanish",
		"italian", "mexican", "chinese", "british", "thai", "vietnamese",
		"cajun_creole", "brazilian", "french", "japanese", "irish", 
		"korean", "moroccan", "russian")

# Each list has $id, $cuisine, $ingredients
# Predict the $cuisine id of elements in the test set

select <- rbinom(n=length(document), size=1, p=.95)
trainSet <- document[select==1]
testSet <- document[select==0]

# We make a dataframe with 21 rows, one for ingredient name, 
# and 20 for the frequency of a specific cuisine.

x <- 7500

ingredient <- character(x)
greek <- numeric(x)
southern_us <- numeric(x)
filipino <- numeric(x)
indian <- numeric(x)
jamaican <- numeric(x)
spanish <- numeric(x)
italian <- numeric(x)
mexican <- numeric(x)
chinese <- numeric(x)
british <- numeric(x)
thai <- numeric(x)
vietnamese <- numeric(x)
cajun_creole <- numeric(x)
brazilian <- numeric(x)
french <- numeric(x)
japanese <- numeric(x)
irish <- numeric(x)
korean <- numeric(x)
moroccan <- numeric(x)
russian <- numeric(x)


ingredientLength <- 1
counter <- 0

#rm(ingredientFrame)

for(i in 1:length(trainSet)) {
	if(length(trainSet[i][[1]]$ingredients) > 0) {
		for(j in 1:length(trainSet[i][[1]]$ingredients)) {
			ingredient[ingredientLength] <- trainSet[i][[1]]$ingredients[j]
			for(k in (i+1):length(trainSet)) {
				if(ingredient[ingredientLength] %in% trainSet[k][[1]]$ingredients) {
					counter <- counter+1
					currentCuisine <- trainSet[k][[1]]$cuisine
					if(currentCuisine == "greek") {
						greek[ingredientLength] <- greek[ingredientLength] + 1
					} else if(currentCuisine == "southern_us") {
						southern_us[ingredientLength] <- southern_us[ingredientLength] + 1
					} else if(currentCuisine == "filipino") {
						filipino[ingredientLength] <- filipino[ingredientLength] + 1
					} else if(currentCuisine == "indian") {
						indian[ingredientLength] <- indian[ingredientLength] + 1
					} else if(currentCuisine == "jamaican") {
						jamaican[ingredientLength] <- jamaican[ingredientLength] + 1
					} else if(currentCuisine == "spanish") {
						spanish[ingredientLength] <- spanish[ingredientLength] + 1
					} else if(currentCuisine == "italian") {
						italian[ingredientLength] <- italian[ingredientLength] + 1
					} else if(currentCuisine == "mexican") {
						mexican[ingredientLength] <- mexican[ingredientLength] + 1
					} else if(currentCuisine == "chinese") {
						chinese[ingredientLength] <- chinese[ingredientLength] + 1
					} else if(currentCuisine == "british") {
						british[ingredientLength] <- british[ingredientLength] + 1
					} else if(currentCuisine == "thai") {
						thai[ingredientLength] <- thai[ingredientLength] + 1
					} else if(currentCuisine == "vietnamese") {
						vietnamese[ingredientLength] <- vietnamese[ingredientLength] + 1
					} else if(currentCuisine == "cajun_creole") {
						cajun_creole[ingredientLength] <- cajun_creole[ingredientLength] + 1
					} else if(currentCuisine == "brazilian") {
						brazilian[ingredientLength] <- brazilian[ingredientLength] + 1
					} else if(currentCuisine == "french") {
						french[ingredientLength] <- french[ingredientLength] + 1
					} else if(currentCuisine == "japanese") {
						japanese[ingredientLength] <- japanese[ingredientLength] + 1
					} else if(currentCuisine == "irish") {
						irish[ingredientLength] <- irish[ingredientLength] + 1
					} else if(currentCuisine == "korean") {
						korean[ingredientLength] <- korean[ingredientLength] + 1
					} else if(currentCuisine == "moroccan") {
						moroccan[ingredientLength] <- moroccan[ingredientLength] + 1
					} else if(currentCuisine == "russian") {
						russian[ingredientLength] <- russian[ingredientLength] + 1
					} 

					trainSet[k][[1]]$ingredients <- trainSet[k][[1]]$ingredients[trainSet[k][[1]]$ingredients != ingredient[ingredientLength]]
				}
			}
			ingredientLength <- ingredientLength + 1
		}
	}
	#print(i)
}

ingredientFrame <- data.frame("ingredient"=ingredient, "greek"=greek, "southern_us"=southern_us, "filipino"=filipino, "indian"=indian, 
				"jamaican"=jamaican, "spanish"=spanish, "italian"=italian, "mexican"=mexican, "chinese"=chinese,
				"british"=british, "thai"=thai, "vietnamese"=vietnamese, "cajun_creole"=cajun_creole, 
				"brazilian"=brazilian, "french"=french, "japanese"=japanese, "irish"=irish, "korean"=korean, 
				"moroccan"=moroccan, "russian"=russian, stringsAsFactors=FALSE)

if(length(ingredientFrame) == ingredientLength) {
	print("Chance of error")
} else if(length(ingredientFrame) > ingredientLength) {
	ingredientFrame <- head(ingredientFrame, ingredientLength)
}

powRowMatrix <- matrix(nrow=ingredientLength, ncol=20)
upperBound <- 2.2
mediumBound <- 1.5
#for(i in 1:ingredientLength) {
for(i in 1:ingredientLength) {
	powRow <- as.numeric(ingredientFrame[i,2:21]) + 1
  if(max(powRow) == 1) {
    ratio1 <- 1
    ratio2 <- 1
    ratio3 <- 1
  }
  else {
    position <- which(powRow == max(powRow))
    ratio <- max(powRow)
    powRow2 <- powRow[powRow != max(powRow)]
    ratio <- ratio/max(powRow2)    
    if(max(powRow2) == 1) {
      ratio2 <- 1
      ratio3 <- 1
    }
    else {
      position2 <- which(powRow == max(powRow2))
      ratio2 <- max(powRow2)
      powRow3 <- powRow2[powRow2 != max(powRow2)]
      ratio2 <- ratio2/max(powRow3)
      if(ratio2 > upperBound) {
        ratio2 <- (3*ratio + 1)/4
      } else if(ratio2 > mediumBound) {
        ratio2 <- (ratio+1)/2
      } else {
        ratio2 <- (ratio+3)/4
      }
      if(max(powRow3) == 1) {
        ratio3 <- 1
      }
      else {
        position3 <- which(powRow == max(powRow3))
        ratio3 <-max(powRow3)
        powRow4 <- powRow3[powRow3 != max(powRow3)]
        ratio3 <- ratio3/max(powRow4)
        if(ratio3 > upperBound) {
          ratio3 <- (3*ratio2+1)/4
        } else if(ratio3 > mediumBound) {
          ratio3 <- (ratio2 + 1)/2
        } else {
          ratio3 <- (ratio2 + 3)/4
        }
      }
    }
  }
	powRow <- rep(1, 20)
	powRow[position] <- ratio
	powRow[position2] <- ratio2
	powRow[position3] <- ratio3
	powRow <- powRow - 1
  powRowMatrix[i,] <- powRow
	#print(i)
}


sampleSize <- length(testSet)
#randomRecipe <- floor( runif(sampleSize, 1, length(document)) )
correctSamples <- numeric(sampleSize)

predictedCuisine <- character(sampleSize)
for(i in 1:sampleSize) {
	count <- 0
	ingrRow <- numeric(length(testSet[i][[1]]$ingredients))
	for(j in 1:length(testSet[i][[1]]$ingredients)) {
		currentIngredient <- testSet[i][[1]]$ingredients[j]
		if(currentIngredient %in% ingredientFrame[,1]) {
			count <- count + 1
			#ingrRow[count] <- which(ingredientFrame == ingredient)
			ingrRow[count] <- which(ingredient == currentIngredient)
		}
	}
	sums <- numeric(20)
	for(j in 1:20) {		#Columns
    sums[j] <- sum(powRowMatrix[ingrRow,j])
    #sums[j] <- sums[j] + sum()
		#for(k in 1:count) {	#Rows
			#sums[j] <- (sums[j] + ingredientFrame[ingrRow[k], j+1])
		#}
	}
	if(length(cuisines[which(sums==max(sums))]) == 1) {
    predictedCuisine[i] <- cuisines[which(sums == max(sums))] 
	}
  else {
    pred <- cuisines[which(sums==max(sums))]
    predictedCuisine[i] <- pred[1]
  }
	if(predictedCuisine[i] == testSet[i][[1]]$cuisine) {
		correctSamples[i] <- 1
	}
	
  #print(i)
	# Later implement: if there is a low difference between 1st place and 2nd place, start looking at individual ingredients
}

#if(!(0 %in% correctSamples)) { print(1) } else { print(0) }
table(correctSamples)
prob <- length(correctSamples[correctSamples == 1])/length(correctSamples)
