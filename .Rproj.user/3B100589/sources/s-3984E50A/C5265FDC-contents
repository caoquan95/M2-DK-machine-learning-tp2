install.packages("RColorBrewer") # coloring the plot
library("RColorBrewer")

X1 <- cbind(rnorm(50, 10, 20), rnorm(50, 10, 10))
X2 <- cbind(rnorm(30, 100, 20), rnorm(30, 100, 20))
X3 <- cbind(rnorm(20, 3, 5), rnorm(20, 120, 5))
X <- rbind(X1, X2, X3)
plot(X)

euclideanDist <- function (x1, x2) {
    return (sqrt( sum((x1 - x2) ^ 2 )))
}

kMeanPlusInit <- function (X, k = 3) {
    # Implementation follows the paper at 
    # http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf
    
    # Choose one center uniformly at random from among the data points.
    rIndex = sample(nrow(X), size = 1)
    centroids = rbind(X[rIndex,])
    
    minDists = NULL
    
    while (nrow(centroids) < k) {
        cat("init centroid: ",  nrow(centroids) + 1, "\n")
        
        # For every observation, compute the distance to the closest centroid
        centroid = centroids[nrow(centroids), ]
        dist = cbind(apply(X, 1, function (x) {euclideanDist(x, centroid)}))
        
        # Update the min distance of every observation to consider the new added centroid
        minDists = apply(cbind(minDists, dist), 1, min)
        
        # Compute the probability base on the distance
        minDistsSquare = minDists ^ 2
        sumMinDistsSquare = sum(minDistsSquare)
        probs =  minDistsSquare / sumMinDistsSquare
        
        # Choose the new centroid base on the computed probability
        newCentroidIndex = sample(nrow(X), 1, prob = probs)
        newCentroid = X[newCentroidIndex, ]
        
        centroids = rbind(centroids, newCentroid)
    }
    return (centroids)
}    
    

kmeansPlus <- function (X, k = 3, threshold = 1e-3) {

    # convert X to matrix    
    X = as.matrix(X)
    
    # choose k observations randomly from X a centroids
    centroids = kMeanPlusInit(X, k)
    # centroidIds = sample(nrow(X), k, replace = FALSE)
    # centroids = as.matrix(X[centroidIds,])
    
    diff = threshold * 2
    
    # distance to the closest centroid for each observation
    minDist = matrix(Inf, nrow=nrow(X), ncol = 1)
    
    # corresponding centroid index for each obervation
    minIndices = matrix(0, nrow = nrow(X), ncol = 1)
    
    while (diff > threshold) {
        # compute the distances between centroids and all observations
        # dists = matrix(0, nrow = nrow(X), ncol = k) 
        for (i in 1:k) {
            cat("Update centroid: ", i, "\n")
            centroid = centroids[i, ]
            
            # Compute the distance of centroid i to all observations
            dist = apply(X, 1, function (x) {euclideanDist(x, centroid)})
            
            # For each observation, update its minIndices and minDist if 
            # its euclidean distance is smaller than the current min distance in the minDist
            minIndices[dist < minDist] = i
            minDist[dist < minDist,] = dist[dist < minDist]
        }
        
        # store the old centroids for later diff computation
        prevCentroids = centroids
        
        # update the centroids
        for (i in 1:k) {
            centroids[i,] = colMeans(as.matrix(X[minIndices == i,]))
        }
        
        diff = sqrt(sum((centroids - prevCentroids)^2)) / k
        cat("Diff: ", diff, "\n")
    }
    
    return (list("centroids" = centroids, "assignment" = minIndices, "diff" = diff))
}


kmeansPlot <- function (X, centroids, assigment) {
    
    # create the color pallete for fancier plot
    pallete = brewer.pal(n = nrow(centroids), name = "Dark2")

    # plot the observations coloring base on assigments
    plot(X, col=pallete[assigment], pch=21)
    
    # plot the centroids
    points(centroids, col = "black", pch=15)
}

kmeansTest <- function (X, k = 3) {
    res <- kmeansPlus(X, k)
    kmeansPlot(X, res$centroids, res$assignment)
}

kmeansTest(X)

plotAudio <- function (audio) {
    audio = as.matrix(audio)
    plot(cbind(1:nrow(audio), audio), type="l", ylab="Value", xlab="Time")    
}

encodeAudio <- function (audio, k) {
    res = kmeansPlus(audio1, k)
    assignment = res$assignment
    centroids = res$centroids
    encodedAudio = centroids[assignment]    
    return (round(encodedAudio))
}


processAudio <- function (inputFilename, k, outputPath) {
    audio1 = as.matrix(read.table(inputFilename))
    # visualize the data
    originImageName = paste(outputPath, "origin.png", sep="") 
    png(filename= originImageName, width = 1920, height = 1080)
    plotAudio(audio1)
    dev.off()
    
    encodedAudio1 = encodeAudio(audio1, k)
    
    outputImageName = paste(outputPath, "k", k, ".png", sep="")
    png(filename=outputImageName, width = 1920, height = 1080)
    plotAudio(encodedAudio1)
    dev.off()
    
    outputFilename = paste(outputPath, "k", k, ".integer", sep="")
    write.table(encodedAudio1, file = outputFilename , row.names = FALSE, col.names=FALSE)
}

processAudio("data/au1.integer", 5, "/home/quan/dataset/audio/audio1/k5/")

