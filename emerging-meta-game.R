library(ggplot2)
n.players <- 40
S <- 0
Te <- 3
H <- -1
P <- -9
E <- -2
n.rounds <- 4
n.generations <- 200
mutation.rate <- 0.01
meta.g <- 1
other.g <- 0
learning.rate <- 0.05
n.games<-10
max.meta <- n.players
max.tempt <- 20
min.tempt <- -4
max.punish <- 5
min.punish <- -10
max.hurt <- 10
min.hurt <- -5
max.enforce <- 10
min.enforce <- -10
increment <- 2.5

start.players <- data.frame(B=runif(n.players, 0, 1),
                            V=runif(n.players, 0, 1),
                            C=runif(n.players, 0, 1),
                            Points=numeric(n.players),
                            Cheated=logical(n.players),
                            Group=numeric(n.players),
                            Punishment=numeric(n.players))


game.data <- data.frame(generation=seq(1,n.generations,1),
                             mean.B=numeric(n.generations),
                             mean.V=numeric(n.generations),
                             mean.C=numeric(n.generations),
                             mean.P.total=numeric(n.generations),
                             mean.P.meta=numeric(n.generations),
                             mean.P.other=numeric(n.generations),
                             num.defectors=numeric(n.generations))

set.data <- data.frame(set=seq(1,n.games,1),
                        mean.B=numeric(n.games),
                        mean.V=numeric(n.games),
                        mean.C=numeric(n.games),
                        mean.P.total=numeric(n.games),
                        mean.P.meta=numeric(n.games),
                        mean.P.other=numeric(n.games),
                        num.defectors=numeric(n.games))




counting <- function(players){
  return(nrow(players))
}

set.inital.meta <- function(players, n.meta){
  for(i in 1:n.meta){
    players$Group[i] <- meta.g
  }
  return(players)
}

grouping <- function(players){
  for(i in 1:n.players){
    coop <- runif(1,0,1)
    if(coop <= players$C[i]){
      players$Group[i] <- meta.g
    }
    else{
      players$Group[i] <- other.g
    }
  }
  return(players)
}

defection <- function(i,S,players){
  # If the players boldness is greater than the chance of being caught, it will cheat.
  if(players$B[i] > S){
    # Gives player i defection points, hurts all other players, and sets i's cheating status to true.
    players$Points[i] <- players$Points[i] + Te
    players$Points[-i] <- players$Points[-i] + H
    players$Cheated[i] <- TRUE
  }
  return(players)
}

vengeance <- function(i,S,players){
  # Itterates all other players to see if they catch i cheating.
  for(j in 1:n.players){
    if(i != j){
      # If player j catches player i:
      caught <- runif(1,0,1)
      if(caught <= S){
        # If player j gets revenge on player i:
        revenge <- runif(1,0,1)
        if(revenge<=players$V[j]){
          # i is punished and j pays the enforcement cost.
          players$Points[i] <- players$Points[i] + P
          players$points[j] <- players$points[j] + E
        }
        else{
          if(players$Group[i]==meta.g && players$Group[j]==meta.g){
            players<- meta.vengeance(i,j,S,players)
          }
        }
      } 
    }
  }
  return(players)
}

meta.vengeance <- function(i,j,S,players){
  for(k in 1:n.players){
    if(k != i && k != j && players$Group[k]==meta.g){
      # If player k catches player j:
      caught <- runif(1,0,1)
      if(caught <= S){
        # If player k gets meta revenge on player j:
        revenge <- runif(1,0,1)
        if(revenge <= players$V[k]){
          # j is punished and k pays the enforcement cost.
          players$Points[j] <- players$Points[j] + P
          players$points[k] <- players$points[k] + E
        }
      } 
    }
  }
  return(players)
}

round <- function(players){
  # Chance of being caught in the round is a rand dist between 0 and 1.
  S <- runif(1,0,1)
  # For each player:
  for(i in 1:n.players){
    players<-defection(i,S,players)
    if(players$Cheated[i] == TRUE){
      players<-vengeance(i,S,players)
    }
    players$Cheated[i]<-FALSE
  }
  return(players)
}

generation <- function(players){
  for(r in 1:n.rounds){
    players<-round(players)
  }
  return(players)
}

reproduction <- function(players){
  new.players <- players
  ranking.players <- players[order(-players$Points),]
  # Itterates until players have reproduced 20 times.
  i <- 1
  while(i <= n.players){
    # for each player, gives a chance to reproduce based on points ranking. 
    # the 1st place player will have a 1/n.players chance of not reproducing,
    # while the last place player will have a 100% chance of not reproducing.
    for(j in 1:n.players){
      chance.reproduction <- runif(1,0,1)
      if(j/n.players <= chance.reproduction){
        new.players[i,] <- ranking.players[j,]
        i <- i + 1
        if(i == n.players){ 
          j <- n.players
        }
      }
    }
  }
  while(nrow(new.players)>n.players){
    new.players <- new.players[-nrow(new.players),]
  }
  return(new.players)
}

social.proof <- function(players,n.group){
  players <- players[order(-players$Points),]
  for(i in 1:n.group){
    r.mutate <- runif(1,0,1)
    if(r.mutate <= mutation.rate){
      BorV <- runif(1,0,1)
      if(BorV >= 0.5){
        players$B[i] <- players$B[i] + (players$B[1]- players$B[i])*learning.rate
        if(players$B[i] > 1){
          players$B[i] <- 1
        }
        if(players$B[i] < 0){
          players$B[i] <- 0
        }
      }
      else{
        players$V[i] <- players$V[i] + (players$V[1]- players$V[i])*learning.rate
        if(players$V[i] > 1){
          players$V[i] <- 1
        }
        if(players$V[i] < 0){
          players$V[i] <- 0
        }
      }
    }
  }
  return(players)
}

mutation <- function(players){
  for(i in 1:n.players){
    r.mutate <- runif(1,0,1)
    if(r.mutate <= mutation.rate){
      BorV <- runif(1,0,1)
      if(BorV >= 0.67){
        players$B[i] <- players$B[i] + runif(1,-0.1,0.1)
        if(players$B[i] > 1){
          players$B[i] <- 1
        }
        if(players$B[i] < 0){
          players$B[i] <- 0
        }
      }
      if(BorV > 0.33 && BorV < 0.67){
        players$V[i] <- players$V[i] + runif(1,-0.1,0.1)
        if(players$V[i] > 1){
          players$V[i] <- 1
        }
        if(players$V[i] < 0){
          players$V[i] <- 0
        }
      }
      if(BorV <= 0.33){
        players$C[i] <- players$C[i] + runif(1,-0.1,0.1)
        if(players$C[i] > 1){
          players$C[i] <- 1
        }
        if(players$C[i] < 0){
          players$C[i] <- 0
        }
      }
    }
  }
  return(players)
}


game <- function(players){
  
  # For each generation:
  for(g in 1:n.generations){
    players<-grouping(players)
    players <- generation(players)
    game.data$mean.B[g] <<- mean(players$B)
    game.data$mean.V[g] <<- mean(players$V)
    game.data$mean.C[g] <<- mean(players$C)
    game.data$mean.P.total[g]<<- mean(sum(players$Points))
    meta.players<- players[which(players$Group == meta.g),]
    other.players<- players[which(players$Group == other.g),]
    game.data$mean.P.meta[g]<<- mean(sum(meta.players$Points))
    game.data$mean.P.other[g]<<-mean(sum(other.players$Points))
    game.data$num.defectors[g]<<-nrow(players[which(players$V > 0.75),])
    
    players<-reproduction(players)
    players<-mutation(players)
    
  }
}





n.tempt <- length(seq(min.tempt,max.tempt,increment)) 
epoch.t <- data.frame(n.tempt=seq(1,n.tempt,1),
                         mean.B=numeric(n.tempt),
                         mean.V=numeric(n.tempt),
                         mean.C=numeric(n.tempt),
                         mean.P.total=numeric(n.tempt),
                         mean.P.meta=numeric(n.tempt),
                         mean.P.other=numeric(n.tempt),
                         num.defectors=numeric(n.tempt),
                         tempt=seq(min.tempt,max.tempt,increment))

epoch.tempt <- function(players,n.games,max.meta,min.meta,increment){
  
  pb <- txtProgressBar(min=1, max=max.meta-min.meta, style=3)
  for(t in 1:n.tempt){
    for(g in 1:n.games){
      Te <<- epoch.t$tempt[t]
      game(start.players)
      set.data$mean.B[g] <<- game.data$mean.B[n.generations]
      set.data$mean.V[g] <<- game.data$mean.V[n.generations]
      set.data$mean.C[g] <<- game.data$mean.C[n.generations]
    }
    epoch.t$mean.B[t]<<- mean(set.data$mean.B)
    epoch.t$mean.V[t]<<- mean(set.data$mean.V)
    epoch.t$mean.C[t]<<- mean(set.data$mean.C)
    setTxtProgressBar(pb, t)
  }
  Te <<- 3
  return(epoch.t)
}

n.punish <- length(seq(min.punish,max.punish,increment)) 
epoch.p <- data.frame(n.punish=seq(1,n.punish,1),
                      mean.B=numeric(n.punish),
                      mean.V=numeric(n.punish),
                      mean.C=numeric(n.punish),
                      mean.P.total=numeric(n.punish),
                      mean.P.meta=numeric(n.punish),
                      mean.P.other=numeric(n.punish),
                      num.defectors=numeric(n.punish),
                      punish=seq(min.punish,max.punish,increment))

epoch.punish <- function(players,n.games,max.meta,min.meta,increment){
  
  pb <- txtProgressBar(min=1, max=max.meta-min.meta, style=3)
  for(t in 1:n.punish){
    for(g in 1:n.games){
      P <<- epoch.p$punish[t]
      game(start.players)
      set.data$mean.B[g] <<- game.data$mean.B[n.generations]
      set.data$mean.V[g] <<- game.data$mean.V[n.generations]
      set.data$mean.C[g] <<- game.data$mean.C[n.generations]
    }
    epoch.p$mean.B[t]<<- mean(set.data$mean.B)
    epoch.p$mean.V[t]<<- mean(set.data$mean.V)
    epoch.p$mean.C[t]<<- mean(set.data$mean.C)
    setTxtProgressBar(pb, t)
  }
  P <<- -9
  return(epoch.p)
}

n.hurt <- length(seq(min.hurt,max.hurt,increment)) 
epoch.h <- data.frame(n.hurt=seq(1,n.hurt,1),
                      mean.B=numeric(n.hurt),
                      mean.V=numeric(n.hurt),
                      mean.C=numeric(n.hurt),
                      mean.P.total=numeric(n.hurt),
                      mean.P.meta=numeric(n.hurt),
                      mean.P.other=numeric(n.hurt),
                      num.defectors=numeric(n.hurt),
                      hurt=seq(min.hurt,max.hurt,increment))

epoch.hurt <- function(players,n.games,max.meta,min.meta,increment){
  
  pb <- txtProgressBar(min=1, max=max.meta-min.meta, style=3)
  for(t in 1:n.hurt){
    for(g in 1:n.games){
      H <<- epoch.h$hurt[t]
      game(start.players)
      set.data$mean.B[g] <<- game.data$mean.B[n.generations]
      set.data$mean.V[g] <<- game.data$mean.V[n.generations]
      set.data$mean.C[g] <<- game.data$mean.C[n.generations]
    }
    epoch.h$mean.B[t]<<- mean(set.data$mean.B)
    epoch.h$mean.V[t]<<- mean(set.data$mean.V)
    epoch.h$mean.C[t]<<- mean(set.data$mean.C)
    setTxtProgressBar(pb, t)
  }
  H <<- -1
  return(epoch.h)
}

n.enforce <- length(seq(min.enforce,max.enforce,increment)) 
epoch.e <- data.frame(n.enforce=seq(1,n.enforce,1),
                      mean.B=numeric(n.enforce),
                      mean.V=numeric(n.enforce),
                      mean.C=numeric(n.enforce),
                      mean.P.total=numeric(n.enforce),
                      mean.P.meta=numeric(n.enforce),
                      mean.P.other=numeric(n.enforce),
                      num.defectors=numeric(n.enforce),
                      enforce=seq(min.enforce,max.enforce,increment))

epoch.enforce <- function(players,n.games,max.meta,min.meta,increment){
  
  pb <- txtProgressBar(min=1, max=max.meta-min.meta, style=3)
  for(t in 1:n.enforce){
    for(g in 1:n.games){
      E <<- epoch.e$enforce[t]
      game(start.players)
      set.data$mean.B[g] <<- game.data$mean.B[n.generations]
      set.data$mean.V[g] <<- game.data$mean.V[n.generations]
      set.data$mean.C[g] <<- game.data$mean.C[n.generations]
    }
    epoch.e$mean.B[t]<<- mean(set.data$mean.B)
    epoch.e$mean.V[t]<<- mean(set.data$mean.V)
    epoch.e$mean.C[t]<<- mean(set.data$mean.C)
    setTxtProgressBar(pb, t)
  }
  E <<- -2
  return(epoch.e)
}





e.tempt <- epoch.tempt(start.players,n.games,max.tempt,min.tempt,increment)
e.punish <- epoch.punish(start.players,n.games,max.punish,min.punish)
e.hurt <- epoch.hurt(start.players,n.games,max.hurt,min.hurt)
e.enforce <- epoch.enforce(start.players,n.games,max.enforce,min.enforce)

ggplot(data=epoch.t,aes(x=tempt,y=mean.V)) +geom_point() +geom_smooth()
ggplot(data=epoch.t,aes(x=tempt,y=mean.B)) +geom_point() +geom_smooth()
ggplot(data=epoch.t,aes(x=tempt,y=mean.C)) +geom_point() +geom_smooth()

ggplot(data=epoch.h,aes(x=hurt,y=mean.V)) +geom_point() +geom_smooth()
ggplot(data=epoch.h,aes(x=hurt,y=mean.B)) +geom_point() +geom_smooth() 
ggplot(data=epoch.h,aes(x=hurt,y=mean.C)) +geom_point() +geom_smooth()

ggplot(data=epoch.p,aes(x=punish,y=mean.V)) +geom_point() +geom_smooth()
ggplot(data=epoch.p,aes(x=punish,y=mean.B)) +geom_point() +geom_smooth()
ggplot(data=epoch.p,aes(x=punish,y=mean.C)) +geom_point() +geom_smooth()

ggplot(data=epoch.e,aes(x=enforce,y=mean.V)) +geom_point() +geom_smooth()
ggplot(data=epoch.e,aes(x=enforce,y=mean.B)) +geom_point() +geom_smooth()
ggplot(data=epoch.e,aes(x=enforce,y=mean.C)) +geom_point() +geom_smooth()
