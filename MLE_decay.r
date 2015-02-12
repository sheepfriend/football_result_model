setwd("/Users/xingyue/Dropbox/football/PL2011-12")
d<-read.csv("EL_no_red.csv",sep=",",header=T,stringsAsFactors=F)
d<-d[,-1]
d<-d[-which(abs(d[,26]-d[,27])>1.25),]
d<-d[-which(d[,26]+d[,27]>2.8),]

len<-length(d[,1])
total.match<-length(d[,1])

lambda.home<-d[,26]
lambda.away<-d[,27]

temp<-which(lambda.home<lambda.away)
if(length(temp)!=0){d[temp,]<-d[temp,c(1,3,2,5,4,16:25,6:15,27,26)]}

lambda.home<-d[,26]
lambda.away<-d[,27]
score.home<-d[,4]
score.away<-d[,5]
boundary<-3:1

add.bound<-function(x){
	boundary<-c(boundary,rep(0,20))
	bound<-cumsum(c(0,boundary))
	y<-matrix(0,20,20)
	for(j in 1:20){
		for(i in 1:20){
			if(boundary[j]>=i){y[i,j]<-x[bound[j]+i]}
			else{
				if(j==1){y[i,j]<-y[i-1,j]}
				else if(i==1){y[i,j]<-y[i,j-1]}
				else{y[i,j]<-y[i-1,j-1]}
			}
		}
	}
	return(y)
}

score.time<-d[,6:25]
score.time.total<-rep(0,90)

for(i in 1:20){				##find the number of scores got in one particular minute
	for(j in 1:total.match){
		if(score.time[j,i]<1000){
			score.time.total[score.time[j,i]]<-score.time.total[score.time[j,i]]+1
		}
	}
}

decay.empirical<-rep(0,90)		##find the empirical decay data
decay.empirical[89]<-score.time.total[90]
for(i in 88:1){decay.empirical[i]<-decay.empirical[i+1]+score.time.total[i+1]}

total<-decay.empirical[1]+score.time.total[1]		##total number of scores

score.time.which<-matrix(0,nrow=total.match,ncol=20)		##record the one which get the score
for(i in 1:10){score.time.which[which(score.time[,i]<1000),i]<-1}	##(1,1,1,...0,2,2,2,...)
for(i in 11:20){score.time.which[which(score.time[,i]<1000),i]<-2}	##needs to be sorted by order of time of score

for(i in 1:total.match){		##add noise
	temp<-which(score.time[i,]<1000)
	if(length(temp)!=0){score.time[i,temp]<-score.time[i,temp]+runif(length(temp))-1}
}

for(i in 1:total.match){
	orders<-order(score.time[i,])
	score.time[i,]<-score.time[i,orders]
	score.time.which[i,]<-score.time.which[i,orders]
}

score.time.home<-matrix(0,nrow=total.match,ncol=20)
score.time.away<-matrix(0,nrow=total.match,ncol=20)

##record the current+1 score for home and away team
for(i in 1:total.match){
	if(score.time.which[i,1]==1){score.time.home[i,1]<-1}
	else if(score.time.which[i,1]==2){score.time.away[i,1]<-1}
	for(j in 2:20){
		if(score.time.which[i,j]==1){
			score.time.home[i,j]<-score.time.home[i,j-1]+1
			score.time.away[i,j]<-score.time.away[i,j-1]
		}
		else if(score.time.which[i,j]==2){
			score.time.home[i,j]<-score.time.home[i,j-1]
			score.time.away[i,j]<-score.time.away[i,j-1]+1
		}
		else{
			score.time.home[i,j]<-score.time.home[i,j-1]
			score.time.away[i,j]<-score.time.away[i,j-1]
		}
	}
}

score.time.home<-cbind(0,score.time.home)[,1:20]
score.time.away<-cbind(0,score.time.away)[,1:20]
score.time<-as.matrix(score.time)
score.time[which(score.time==1000)]<-90

##compute the total decay at each time point
decay.assump<-function(x,decay){
	b<-decay[1]
	c<-decay[2]
	d<-decay[3]
	e<-decay[4]
	g<-decay[5]
	h<-decay[6]
	i<-decay[7]
	a<-b*0.6
	f<-g*0.6
	j<-1-5.6*b-15*c-23*d-e-5.6*g-29*h-9*i
	x[which(x<=1)]<-x[which(x<=1)]*a
	x[which(x>1 & x<=6)]<-(x[which(x>1 & x<=6)]-1)*b+a
	x[which(x>6 & x<=21)]<-(x[which(x>6 & x<=21)]-6)*c+5*b+a
	x[which(x>21 & x<=44)]<-(x[which(x>21 & x<=44)]-21)*d+15*c+5*b+a
	x[which(x>44 & x<=45)]<-(x[which(x>44 & x<=45)]-44)*e+23*d+15*c+5*b+a
	x[which(x>45 & x<=46)]<-(x[which(x>45 & x<=46)]-45)*f+e+23*d+15*c+5*b+a
	x[which(x>46 & x<=51)]<-(x[which(x>46 & x<=51)]-46)*g+f+e+23*d+15*c+5*b+a
	x[which(x>51 & x<=80)]<-(x[which(x>51 & x<=80)]-51)*h+5*g+f+e+23*d+15*c+5*b+a
	x[which(x>80 & x<=89)]<-(x[which(x>80 & x<=89)]-80)*i+29*h+5*g+f+e+23*d+15*c+5*b+a
	x[which(x>89)]<-(x[which(x>89)]-89)*j+9*i+29*h+5*g+f+e+23*d+15*c+5*b+a
	return(x)
}

##compute the decay of the current time
decay.current<-function(x,decay){
	b<-decay[1]
	c<-decay[2]
	d<-decay[3]
	e<-decay[4]
	g<-decay[5]
	h<-decay[6]
	i<-decay[7]
	a<-b*0.6
	f<-g*0.6
	j<-1-5.6*b-15*c-23*d-e-5.6*g-29*h-9*i
	y<-rep(0,length(x))
	y[which(x<=1)]<-a
	y[which(x<=6 & x>1)]<-b
	y[which(x<=21 & x>6)]<-c
	y[which(x<=44 & x>21)]<-d
	y[which(x<=45 & x>44)]<-e
	y[which(x<=46 & x>45)]<-f
	y[which(x<=51 & x>46)]<-g
	y[which(x<=80 & x>51)]<-h
	y[which(x<=89 & x>80)]<-i
	y[which(x>89)]<-j
	return(y)
}

score.bound<<-c(3,2,1,rep(0,10))

##print lambda matrix (the first print is not the result)
result.lambda<-function(x){
	sup.a<-add.bound(x[1:15])
	sup.b<-add.bound(x[16:30])
	ttg.a<-add.bound(x[31:45])
	ttg.b<-add.bound(x[46:60])
	print(sup.a[1:5,1:5])
	print(sup.b[1:5,1:5])
	print(ttg.a[1:5,1:5])
	print(ttg.b[1:5,1:5])
	lambda<-array(0,dim=c(2,10,10,length(lambda.home)))
	for(i in 1:10){
		for(j in 1:10){
			if(score.bound[i]>=j){
				sup<-sup.b[i,j]*(lambda.home-lambda.away)+sup.a[i,j]
				ttg<-ttg.b[i,j]*(lambda.home+lambda.away)+ttg.a[i,j]
				lambda[1,i,j,]<-(ttg+sup)/2
				lambda[2,i,j,]<-(ttg-sup)/2
				pre1<-lambda[1,i,j,]
				pre2<-lambda[2,i,j,]	
			}
			else{
				lambda[1,i,j,]<-pre1
				lambda[2,i,j,]<-pre2
			}
		}
	}
	return(lambda)
}

##flow for computing log(L)
## [1]get.prob.decay: compute the decay array(dim=c(11,11,3))
## [2]get.prob: use the decay array and lambda parameters to compute log(L)

##info for decay array
##	c(11,11):ten time point (20 total)
##	3: 1:home team get the score->current.decay
##	3: 2:away team get the score->current.decay
##	3: 3:both team->delta.decay
## decay.assump(90)=1  decay.assump(0)=0

##the options could not be used now...
get.prob<-function(sup.a,sup.b,ttg.a,ttg.b,records,option){
	sup.a<-add.bound(sup.a)
	sup.b<-add.bound(sup.b)
	ttg.a<-add.bound(ttg.a)
	ttg.b<-add.bound(ttg.b)
	lambda<-array(0,dim=c(2,11,11,length(lambda.home)))
	sup1<-lambda
	sup2<-lambda
	pre<-matrix(0,nrow=length(lambda.home),ncol=2) ##the record the path
	for(i in 1:10){
		for(j in 1:10){
			if(i+j>2 |((option==2 | option==1) & i+j==2)){
				if(score.bound[i]>=j){
					sup<-sup.b[i,j]*(lambda.home-lambda.away)+sup.a[i,j]
					ttg<-ttg.b[i,j]*(lambda.home+lambda.away)+ttg.a[i,j]
					sup1[1,i,j,]<-sup
					sup2[1,i,j,]<-ttg
					temp1<-which(score.time.home==i-1 & score.time.away==j-1 & score.time.which==1)
					temp2<-which(score.time.home==i-1 & score.time.away==j-1 & score.time.which==2)
					lambda[1,i,j,]<-(ttg+sup)/2
					lambda[2,i,j,]<-(ttg-sup)/2
					if(length(temp1)!=0){
						temp1<-temp1-trunc((temp1-0.0000001)/len)*len
						sup1[2,i+1,j,temp1]<-sup[temp1]
						sup2[2,i+1,j,temp1]<-ttg[temp1]
						if(score.bound[i]==j){
							pre[temp1,1]<-lambda[1,i,j,temp1]
							pre[temp1,2]<-lambda[2,i,j,temp1]
						}
					}
					if(length(temp2)!=0){
						temp2<-temp2-trunc((temp2-0.0000001)/len)*len
						sup1[2,i,j+1,temp2]<-sup[temp2]
						sup2[2,i,j+1,temp2]<-ttg[temp2]
						if(score.bound[i]==j){
							pre[temp2,1]<-lambda[1,i,j,temp2]
							pre[temp2,2]<-lambda[2,i,j,temp2]
						}
					}
				}
				else{
					lambda[1,i,j,]<-pre[,1]
					lambda[2,i,j,]<-pre[,2]
				}
			}
			else{
				lambda[1,i,j,]<-lambda.home
				lambda[2,i,j,]<-lambda.away
			}
		}
	}
	temp<-which(sup1[1,,,]!=0 & sup1[2,,,]!=0 & abs(sup1[1,,,]-sup1[2,,,])>0.25)
	temp1<-which(sup2[1,,,]!=0 & sup2[2,,,]!=0 & abs(sup2[1,,,]-sup2[2,,,])>0.25)
	if(length(temp)!=0){return(-Inf)}
	if(length(temp1)!=0){return(-Inf)}
	##till now, the lambda array is finished
	##some entries in lambda array is not zero for wrong path, but when multiplying with decay array, the wrong path return 0.
	sum1<-records
	for(i in 1:10){
		for(j in 1:10){
			temp1<-which(sum1[i,j,1,]!=0)
			temp2<-which(sum1[i,j,2,]!=0)
			sum1[i,j,1,temp1]<-log(lambda[1,i,j,temp1]*sum1[i,j,1,temp1])
			sum1[i,j,2,temp2]<-log(lambda[2,i,j,temp2]*sum1[i,j,2,temp2])
			sum1[i,j,3,]<--sum1[i,j,3,]*(lambda[1,i,j,]+lambda[2,i,j,])
		}
	}	
	return(sum(sum1))
}

get.prob.decay<-function(x,option=1){
	bound<-max(boundary)
	bound<-bound*(bound+1)/2
	if(option==1 | option==3){decay<-x[(bound*4+1):(bound*4+7)]}
	else if(option==2 | option==4){decay<-rep(1/90,7)}
	records<-array(0,dim=c(10,10,5,length(lambda.home)))
	score.time.current<-decay.current(score.time,decay)
	score.time<-decay.assump(score.time,decay)
	temp<-cbind(score.time,1)
	temp01<-numeric(0)
	temp02<-numeric(0)
	temp03<-numeric(0)
	score.time1<-cbind(0,score.time)
	score.time.inter<-temp-score.time1
	score.time.inter<-score.time.inter[,1:20]
	records<-array(0,dim=c(11,11,3,length(lambda.home)))
	pre<-rep(0,length(lambda.home))
	for(i in 1:10){
		for(j in 1:10){
			if(score.bound[i]>=j){
				temp<-which(score.time.home==i-1 & (score.time.away==j-1 & score.time.inter!=0))
				if(length(temp)!=0){
					temp1<-which(score.time.which[temp]==1)
					temp2<-which(score.time.which[temp]==2)
					if(length(temp1)!=0){records[i,j,1,temp[temp1]-trunc((temp[temp1]-0.0000001)/len)*len]<-score.time.current[temp[temp1]]}
					if(length(temp2)!=0){records[i,j,2,temp[temp2]-trunc((temp[temp2]-0.0000001)/len)*len]<-score.time.current[temp[temp2]]}
					records[i,j,3,temp-trunc((temp-0.0000001)/len)*len]<-score.time.inter[temp]
					if(score.bound[i]==j){
						pre[temp[temp1]-trunc((temp[temp1]-0.0000001)/len)*len]<-1-score.time[temp[temp1]]
						pre[temp[temp2]-trunc((temp[temp2]-0.0000001)/len)*len]<-1-score.time[temp[temp2]]
						records[i+1,j,3,temp[temp1]-trunc((temp[temp1]-0.0000001)/len)*len]<-1-score.time[temp[temp1]]
						records[i,j+1,3,temp[temp2]-trunc((temp[temp2]-0.0000001)/len)*len]<-1-score.time[temp[temp2]]
					}
				}
			}
			else{
				temp1<-which(score.time.home==i-1 & score.time.away==j-1 & score.time.which==1 & score.time.inter!=0)
				temp2<-which(score.time.home==i-1 & score.time.away==j-1 & score.time.which==2 & score.time.inter!=0)
				if(length(temp1)!=0){records[i+1,j,1,temp1-trunc((temp1-0.0000001)/len)*len]<-pre[temp1-trunc((temp1-0.0000001)/len)*len]}
				if(length(temp2)!=0){records[i,j+1,2,temp2-trunc((temp2-0.0000001)/len)*len]<-pre[temp2-trunc((temp2-0.0000001)/len)*len]}
			}
		}
	}
	return(-get.prob(x[1:bound],x[bound+1:bound],x[bound*2+1:bound],x[bound*3+1:bound],records,option))
}

aic<-function(x){
	num<-length(which(x$par!=1 & x$par!=0))
	return(2*x$obj+2*num)
}

result.read<-function(x){
	result<-list()
	bound<-boundary[1]*(boundary[1]+1)/2
	result[[1]]<-(round(add.bound(x$par[1:bound])[1:3,1:3],4))
	result[[2]]<-(round(add.bound(x$par[(bound+1):(2*bound)])[1:3,1:3],4))
	result[[3]]<-(round(add.bound(x$par[(bound*2+1):(3*bound)])[1:3,1:3],4))
	result[[4]]<-(round(add.bound(x$par[(bound*3+1):(4*bound)])[1:3,1:3],4))
	return(result)
}

de<-c(0.00816641, 0.00915958, 0.00978554, 0.0307267 ,0.0118669 ,0.0113147, 0.0121215)
initial<-list()
initial[[1]]<-c(rep(0.0433,6),rep(1.0234,6),rep(0.0244,6),rep(0.9851,6),de)
initial[[2]]<-c(rep(0.0433,6),rep(1.0234,6),rep(0.0244,6),rep(0.9851,6),rep(1/90,7))
initial[[3]]<-c(0,0.1,0.1,0,0.07,0,1,1,1,0.9,0.84,0.9,0,0.1,0.1,0.1,0,0.1,1,1,1,0.95,1,0.95,de)
initial[[4]]<-c(0,0.1,0.1,0,0.07,0,1,1,1,0.9,0.84,0.9,0,0.1,0.1,0.1,0,0.1,1,1,1,0.95,1,0.95,rep(1/90,7))
initial[[5]]<-c(0.0443,0.02,0.0453,-0.0252,0.0471,-0.0177,1.0234,1.02,1.016,0.9686,1.0216,0.9838,0.0244,0.0059,0.0096,0.0128,0.0228,0.002,0.9851,0.9799,0.9987,0.9798,1.0369,0.9874,de)
initial[[6]]<-c(0.0443,0.02,0.0453,-0.0252,0.0471,-0.0177,1.0234,1.02,1.016,0.9686,1.0216,0.9838,0.0244,0.0059,0.0096,0.0128,0.0228,0.002,0.9851,0.9799,0.9987,0.9798,1.0369,0.9874,rep(1/90,7))
initial[[7]]<-c(0.0433,rep(0,5),1.0234,rep(1,5),0.0244,rep(0,5),0.9851,rep(1,5),de)
initial[[8]]<-c(0.0433,rep(0,5),1.0234,rep(1,5),0.0244,rep(0,5),0.9851,rep(1,5),rep(1/90,7))
initial[[9]]<-c(rep(0.03,6),rep(0.97,6),rep(0.03,6),rep(0.97,6),de)
initial[[10]]<-c(rep(0.03,6),rep(0.97,6),rep(0.03,6),rep(0.97,6),rep(1/90,7))
initial[[11]]<-c(rep(-0.03,6),rep(1.03,6),rep(-0.03,6),rep(1.03,6),de)
initial[[12]]<-c(rep(-0.03,6),rep(1.03,6),rep(-0.03,6),rep(1.03,6),rep(1/90,7))
initial[[13]]<-c(rep(0.03,6),rep(0.97,6),rep(-0.03,6),rep(1.03,6),de)
initial[[14]]<-c(rep(0.03,6),rep(0.97,6),rep(-0.03,6),rep(1.03,6),rep(1/90,7))
initial[[15]]<-c(rep(-0.03,6),rep(1.03,6),rep(0.03,6),rep(0.97,6),de)
initial[[16]]<-c(rep(-0.03,6),rep(1.03,6),rep(0.03,6),rep(0.97,6),rep(1/90,7))

result<-list()
for(i in 1:5){
	result[[i]]<-nlminb(initial[[i]],get.prob.decay,control=list(trace=T))
	print(result[[i]])
}

save(result,file="football.nlminb.rdata")

##sample use of MLE
temp<-nlminb(c(rep(0,6),rep(1,6),rep(0,6),rep(1,6),rep(1/90,7)),get.prob.decay)
