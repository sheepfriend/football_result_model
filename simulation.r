d<-read.csv("EL_no_red.csv",sep=",",header=T,stringsAsFactors=F)
d<-d[,-1]
d<-d[-which(abs(d[,26]-d[,27])>1.25),]
d<-d[-which(d[,26]+d[,27]>4)]

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

score.bound<<-c(3,2,1,rep(0,10))
boundary<-3:1

result1<-matrix(0,5,5)		##calculate the empirical probability matrix
for(i in 1:5){
	for(j in 1:5){
		result1[i,j]<-length(which(score.home[1:total.match]==i-1 & score.away[1:total.match]==j-1))/total.match
	}
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


#compute decay matrix for decay.inv()
decay.result<-c(0.008098374 ,0.009065198 ,0.009666567 ,0.02965158 ,0.01220797 ,0.01131943 ,0.01242136 )
#decay.result<-rep(1/90,7)
decay<-matrix(c(0,1,1,6,6,21,21,44,44,45,45,46,46,51,51,80,80,89,89,90),nrow=2)
decay.inv.matrix<-matrix(0,ncol=10,nrow=4)
for(i in 1:10){
	decay.inv.matrix[1,i]<-decay[1,i]/90
	decay.inv.matrix[2,i]<-decay.assump(decay[1,i],decay.result)
	decay.inv.matrix[3,i]<-decay.assump(decay[2,i],decay.result)
	decay.inv.matrix[4,i]<-decay.current(decay[2,i],decay.result)
}
decay.inv<-function(x){
	y<-x
	for(i in 1:10){
		temp<-which(x>decay.inv.matrix[2,i] & x<=decay.inv.matrix[3,i])
		y[temp]<-decay.inv.matrix[1,i]+(y[temp]-decay.inv.matrix[2,i])/decay.inv.matrix[4,i]/90
	}
	return(y)
}

##	find outliers
xx<-cbind(lambda.home,lambda.away)
cc<-var(xx)
yy<-apply(xx,2,mean)
rr<-mahalanobis(xx,cov=cc,center=yy)
outliers<-which(rr>qchisq(0.99,df=2))
home.clean<-lambda.home[-outliers]
away.clean<-lambda.away[-outliers]
home.out<-lambda.home[outliers]
away.out<-lambda.away[outliers]

#grouping lambda
clean<-function(){
	record<-c()
	home.length<-max(home.clean)-min(home.clean)
	away.length<-max(away.clean)-min(away.clean)
	home.start<-min(home.clean)
	away.start<-min(away.clean)
	delta1<-home.length/8
	delta2<-away.length/8
	for(i in 0:8){
		for(j in 0:8){
			x1<-home.start+i*delta1
			y1<-away.start+j*delta2
			x2<-home.start+(i+1)*delta1
			y2<-away.start+(j+1)*delta2
			if(i<8 & j<8){which<-which(home.clean>=x1 & home.clean<x2 & away.clean>=y1 & away.clean<y2)}
			else if(i==8 & j<8){which<-which(home.clean>=x1 & home.clean<=x2 & away.clean>=y1 & away.clean<y2)}
			else if(i<8 & j==8){which<-which(home.clean>=x1 & home.clean<x2 & away.clean>=y1 & away.clean<=y2)}
			else{which<-which(home.clean>=x1 & home.clean<=x2 & away.clean>=y1 & away.clean<=y2)}
			if(length(which)!=0){
				x<-mean(home.clean[which])
				y<-mean(away.clean[which])
				record<-rbind(record,c(x,y,length(which),i,j,x1,x2,y1,y2))
			}
		}
	}
	return(record)
}

#run simulation and get time array(3D)
simu<-function(home,away,sup.a,sup.b,ttg.a,ttg.b,n){
	sup.a<-add.bound(sup.a)
	sup.b<-add.bound(sup.b)
	ttg.a<-add.bound(ttg.a)
	ttg.b<-add.bound(ttg.b)
	time<-array(0,dim=c(5,6,6,n)) #1:sum 2:exp1 3:exp2 4:which 5:cumdecay
	time[c(2,3),,,]<-rexp(2*6*6*n)
	lambda<-array(0,dim=c(2,6,6,n))
	sup<-sup.a+(home-away)*sup.b
	ttg<-ttg.a+(home+away)*ttg.b
	boundary<-c(boundary,rep(0,10))
	#compute lambdas
	for(i in 1:6){
		for(j in 1:6){
			if(boundary[j]>=i){
				#print(c(i,j))
				lambda[1,i,j,]<-rep((sup[i,j]+ttg[i,j])/2,n)
				lambda[2,i,j,]<-rep((ttg[i,j]-sup[i,j])/2,n)
			}
			if(i+j==2){temp1<-1:n}
			else{temp1<-which(time[5,i,j,]!=0)}
			if(length(temp1)!=0){
				#print(temp1)
				time[2,i,j,temp1]<-time[2,i,j,temp1]/lambda[1,i,j,temp1]
				time[3,i,j,temp1]<-time[3,i,j,temp1]/lambda[2,i,j,temp1]
				#print(c(time[2,i,j,temp1[1]]*lambda[1,i,j,temp1],lambda[1,i,j,temp1]))
				temp<-which(time[2,i,j,temp1]<time[3,i,j,temp1])
				if(length(temp)!=0){
					time[1,i,j,temp1[temp]]<-time[2,i,j,temp1[temp]]
					time[4,i,j,temp1[temp]]<-1
				}
				temp<-which(time[2,i,j,temp1]>=time[3,i,j,temp1])
				if(length(temp)!=0){
					time[1,i,j,temp1[temp]]<-time[3,i,j,temp1[temp]]
					time[4,i,j,temp1[temp]]<-2
				}
				time[5,i,j,temp1]<-time[5,i,j,temp1]+time[1,i,j,temp1]
				if(i<6 & j<6){
					temp<-which(time[4,i,j,temp1]==1)
					if(length(temp)!=0){time[5,i+1,j,temp1[temp]]<-time[5,i,j,temp1[temp]]}
					if(length(temp)!=n){time[5,i,j+1,temp1[-temp]]<-time[5,i,j,temp1[-temp]]}
				}
				if(boundary[j]<=i & i<6 & j<6){
					temp<-which(time[4,i,j,temp1]==1)
					if(length(temp)!=0){
						lambda[1,i+1,j,temp1[temp]]<-lambda[1,i,j,temp1[temp]]
						lambda[2,i+1,j,temp1[temp]]<-lambda[2,i,j,temp1[temp]]
					}
					temp<-which(time[4,i,j,temp1]==2)
					if(length(temp)!=n){
						lambda[1,i,j+1,temp1[temp]]<-lambda[1,i,j,temp1[temp]]
						lambda[2,i,j+1,temp1[temp]]<-lambda[1,i,j,temp1[temp]]
					}
				}
			}
			#for(k in 1:5){print(time[k,,,1])}
		}
	}
	#return(lambda)
	#return(time)
	time[5,,,]<-decay.inv(time[5,,,])
	time.result<-time[5,,,]
	temp<-which(time[4,,,]==0)
	if(length(temp)!=0){time.result[temp]<-0}
	return(time.result)
}

record<-clean()

#use time array to compute scores for each match
prob.result<-function(time_){
	score.record<-rep(0,90)
	for(i in 1:90){score.record[i]<-length(which(time_>(i-1)/90 & time_<=i/90))}
	total<-length(time_[1,1,])
	result<-matrix(0,6,6)
	tag<-rep(0,total)
	score.home<-rep(0,total)
	score.away<-rep(0,total)
	for(i in 1:total){
		temp<-time_[,,i]
		temp1<-which(temp>=1)
		temp2<-which(temp<1)
		if(length(temp2)!=0){tag[i]<-max(temp[temp2])}
		if(length(temp1)!=0){
			temp[temp1]<-0
			time_[,,i]<-temp
		}
	}
	for(i in 1:6){
		for(j in 1:6){
			temp<-which(time_[i,j,]==tag)
			if(length(temp)!=0){
				score.home[temp]<-i
				score.away[temp]<-j
			}
			result[i,j]<-length(temp)
		}
	}
	return(list(result/total,score.record,time_,score.home,score.away))
}

temp<-c(0.04331384 ,-0.02518194 ,-0.01766219 ,0.08909896 ,0.04708757 ,0.04526073 ,1.023437 ,0.968563 ,0.9837945 ,1.053752 ,1.021599 ,1.016034 ,0.02435251 ,0.01275951 ,0.001981997 ,0.005938704 ,0.02282604 ,0.009604253 ,0.9851276 ,0.9798081 ,0.9874448 ,0.9799388 ,1.036947 ,0.9986908 ,0.008098374 ,0.009065198 ,0.009666567 ,0.02965158 ,0.01220797 ,0.01131943 ,0.01242136)

simu.groups<-function(n=100){
	result<-matrix(0,6,6)
	score.record<-rep(0,90)
	time.record<-array(0,dim=c(6,6,length(home.clean)*n))
	score.home<-c()
	score.away<-c()
	for(i in 1:length(home.clean)){
		recording<-prob.result(simu(home.clean[i],away.clean[i],temp[1:6],temp[7:12],temp[13:18],temp[19:24],n))
		result<-result+recording[[1]]
		score.record<-score.record+recording[[2]]
		#print(dim(recording[[3]]))
		time.record[,,((i-1)*n+1):(i*n)]<-recording[[3]]
		score.home<-c(score.home,recording[[4]])
		score.away<-c(score.away,recording[[5]])
	}
	return(list(result/length(home.clean),score.record/length(home.clean),time.record,score.home,score.away))
}

simu.result<-function(x){
	result<-c()
	result.matrix<-matrix(0,6,6)
	for(i in 1:6){
		for(j in 1:6){
			temp<-which(x[[3]][i,j,]>0)
			if(length(temp)!=0){
				sum_lambda<-sum(x[[4]][temp]+x[[5]][temp]-i-j)
				sum_decay<-sum(1-x[[3]][i,j,temp])
				result<-rbind(result,c(i,j,sum_lambda,sum_decay,sum_lambda/sum_decay))
				result.matrix[i,j]<-sum_lambda/sum_decay
			}
		}
	}
	print(result.matrix)
	return(result)
}
