d<-read.csv("/Users/sheepfriend/Dropbox/2013_sem_2/football/football2.csv",sep=",",header=F,stringsAsFactors=F)

d<-d[which(d[,1]<=2008),]
total.match<-length(d[,1])

lambda.home<-d[,26]
lambda.away<-d[,27]

swap<-which(lambda.home<lambda.away)

temp<-d[swap,26]
d[swap,26]<-d[swap,27]
d[swap,27]<-temp

temp<-d[swap,6:15]
d[swap,6:15]<-d[swap,16:25]
d[swap,16:25]<-temp

temp<-d[swap,4]
d[swap,4]<-d[swap,4]
d[swap,5]<-temp

lambda.home<-d[,26]
lambda.away<-d[,27]

score.home<-d[,4]
score.away<-d[,5]

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

get.prob<-function(sup.a,sup.b,ttg.a,ttg.b,records,option,type){
	sup.a<-add.bound(sup.a)
	sup.b<-add.bound(sup.b)
	ttg.a<-add.bound(ttg.a)
	ttg.b<-add.bound(ttg.b)
	lambda<-array(0,dim=c(2,10,10,length(lambda.home)))
	if(type=='sup'){
		for(i in 1:10){
			for(j in 1:10){
				if(i+j>2 |((option==2 | option==1) & i+j==2)){
					sup<-sup.b[i,j]*(lambda.home-lambda.away)+sup.a[i,j]
					ttg<-ttg.b[i,j]*(lambda.home+lambda.away)+ttg.a[i,j]
					lambda[1,i,j,]<-(ttg+sup)/2
					lambda[2,i,j,]<-(ttg-sup)/2
				}
				else{
					lambda[1,i,j,]<-lambda.home
					lambda[2,i,j,]<-lambda.away
				}
			}
		}		
	}
	else if(type=='sep'){
		for(i in 1:10){
			for(j in 1:10){
				if(i+j>2 |((option==2 | option==1) & i+j==2)){
					sup<-sup.b[i,j]*(lambda.home)+sup.a[i,j]
					ttg<-ttg.b[i,j]*(lambda.away)+ttg.a[i,j]
					lambda[1,i,j,]<-sup
					lambda[2,i,j,]<-ttg
				}
				else{
					lambda[1,i,j,]<-lambda.home
					lambda[2,i,j,]<-lambda.away
				}
			}
		}
	}
	else{
		for(i in 1:10){
			for(j in 1:10){
				if(i+j>2 |((option==2 | option==1) & i+j==2)){
					sup<-sup.b[i,j]*(lambda.home)
					ttg<-ttg.b[i,j]*(lambda.away)
					lambda[1,i,j,]<-sup
					lambda[2,i,j,]<-ttg
				}
				else{
					lambda[1,i,j,]<-lambda.home
					lambda[2,i,j,]<-lambda.away
				}
			}
		}
	}
	sum1<-records
	for(i in 1:10){
		for(j in 1:10){
			temp1<-which(sum1[i,j,1,]!=0)
			temp2<-which(sum1[i,j,2,]!=0)
			sum1[i,j,1,temp1]<--sum1[i,j,1,temp1]*(lambda[1,i,j,temp1]+lambda[2,i,j,temp1])+log(lambda[1,i,j,temp1]*sum1[i,j,4,temp1])
			sum1[i,j,2,temp2]<--sum1[i,j,2,temp2]*(lambda[1,i,j,temp2]+lambda[2,i,j,temp2])+log(lambda[2,i,j,temp2]*sum1[i,j,5,temp2])
			sum1[i,j,3,]<--sum1[i,j,3,]*(lambda[1,i,j,]+lambda[2,i,j,])
		}
	}
	return(sum(sum1))
}

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
			sup<-sup.b[i,j]*(lambda.home-lambda.away)+sup.a[i,j]
			ttg<-ttg.b[i,j]*(lambda.home+lambda.away)+ttg.a[i,j]
			lambda[1,i,j,]<-(ttg+sup)/2
			lambda[2,i,j,]<-(ttg-sup)/2
		}
	}
	return(lambda)
}

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

decay.current<-function(x,decay){		##return the decay of the current minute
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

decay.empirical<-function(x){				##define empirical decay function
	temp<-which(floor(x)==x)
	temp1<-which(floor(x)!=x)
	if(length(temp)!=0){x[temp]<-decay.empirical[x[temp]]/total}
	if(length(temp1)!=0){
		interval<-x[temp1]-floor(x[temp1])
		temp2<-which(x[temp1]<1)
		temp3<-which(x[temp1]>90)
		temp4<-which(x[temp1]>=1 & x[temp1]<=90)
		if(length(temp2)!=0){x[temp1[temp2]]<-(total-score.time.total[1]*interval[temp2])/total}
		if(length(temp3)!=0){x[temp1[temp3]]<--score.time.total[90]*interval[temp3]/total}
		if(length(temp4)!=0){x[temp1[temp4]]<-(decay.empirical[floor(x[temp1[temp4]])]-score.time.total[x[temp1[temp4]]+1]*interval[temp4])/total}
	}
	return(x)
}

decay.current.empirical<-function(x){
	y<-ceiling(x)
	y<-matrix(score.time.total[y]/total,ncol=20)
	return(y)
}

get.prob.decay<-function(x,option=1,type="sup"){
	if(option==1 | option==3){decay<-x[61:67]}
	else if(option==2 | option==4){decay<-rep(1/90,7)}
	records<-array(0,dim=c(10,10,5,length(lambda.home)))
	if(option!=5){
		score.time.current<-decay.current(score.time,decay)
		score.time<-decay.assump(score.time,decay)
	}
	else{
		score.time.current<-decay.current.empirical(score.time,decay)
		score.time<-decay.empirical(score.time,decay)
	}
	temp<-cbind(score.time,1)
	score.time1<-cbind(0,score.time)
	score.time.inter<-temp-score.time1
	score.time.inter<-score.time.inter[,1:20]	
	records<-array(0,dim=c(10,10,5,length(lambda.home)))
	for(i in 1:10){
		for(j in 1:10){
			temp<-which(score.time.home==i-1 & (score.time.away==j-1 & score.time.inter!=0))
			if(length(temp)!=0){
				temp1<-which(score.time.which[temp]==1)
				temp2<-which(score.time.which[temp]==2)
				temp3<-which(score.time.which[temp]==0)
				if(length(temp1)!=0){
					records[i,j,1,temp[temp1]-trunc((temp[temp1]-0.0000001)/2660)*2660]<-score.time.inter[temp[temp1]]
					records[i,j,4,temp[temp1]-trunc((temp[temp1]-0.0000001)/2660)*2660]<-score.time.current[temp[temp1]]
				}
				if(length(temp2)!=0){
					records[i,j,2,temp[temp2]-trunc((temp[temp2]-0.0000001)/2660)*2660]<-score.time.inter[temp[temp2]]
					records[i,j,5,temp[temp2]-trunc((temp[temp2]-0.0000001)/2660)*2660]<-score.time.current[temp[temp2]]
				}
				if(length(temp3)!=0)records[i,j,3,temp[temp3]-trunc((temp[temp3]-0.0000001)/2660)*2660]<-score.time.inter[temp[temp3]]
			}
		}
	}
	return(-get.prob(x[1:15],x[16:30],x[31:45],x[46:60],records,option,type))
}

result<-list()
for(i in 1:4){
	print(i)
	boundary<-i:1
	result[[i]]<-nlminb(c(rep(0,45),rep(1,15),rep(1/90,7)),get.prob.decay)
	print(result[[i]])
}

aic<-function(x){
	num<-length(which(x$par!=1 & x$par!=0))
	return(2*x$obj+2*num)
}
