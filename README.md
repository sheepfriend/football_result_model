football_result_model
=====================
@@@@@@@@@@@The part to generate probability matrix is not uploaded yet@@@@@@@@@@@@


get.prob.decay:
x: a 67-para vector where 
          1:15 for sup.a, 16:30 for sup.b
          31:45 for ttg.a, 46:60 for ttg.b
          61:67 are decay parameters
type: 'sup': (lambda1-lambda2)*sup.b+sup.a (lambda1+lambda2)*ttg.b+ttg.a
         'sep': lambda1*sup.b+sup.a           lambda2*ttg.b+ttg.a
         else: lambda1:sup.b                 lambda2*ttg.a
option: 1: assumped decay with lambda(0,0) renewed
           2: 1/90 decay with lambda(0,0) renewed
           3: assumped decay with lambda(0,0) remained
           4: 1/90 decay with lambda(0,0) remained
           5: historical with lambda(0,0) remained

Useage: the last part in football.r is an example to compute the MLE result.
