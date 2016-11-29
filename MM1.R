            
mm1<-function(){
	
lambda = 2
promedio_lambda=(1/lambda)
cat("Tiempo promedio de llegada:",promedio_lambda,"\n")
mu = 4
promedio_mu=(1/mu)
cat("Tiempo promedio del servicio:",promedio_mu,"\n")
cat("\n")


tiempo_limite = 20
tiempo_actual = 0

total_cola = 0
store = 0

tiempo_exp = rexp(1,lambda)
cat("Tiempo que ocurre el evento:",tiempo_exp,"\n")
cola = 1
tiempo_actual = tiempo_exp
numero_de_evento = 1
llegadas=1
servicios=0
cat("Numero de eventos:",numero_de_evento,"\n")
cat("Cola actual:",cola,"\n")
cat("\n")


while (tiempo_actual<tiempo_limite) 
{
numero_de_evento = numero_de_evento+1
cat("Evento #",numero_de_evento,"\n")
  
if(cola>0) 
{
 	tiempo_exp = rexp(1,lambda+mu)
 	cat("Tiempo que ocurre el evento:",tiempo_exp,"\n")
 	p = runif(1,0,1)
    	total_cola[numero_de_evento] = cola 
    	cat("En cola antes del evento:",total_cola[numero_de_evento],"\n")
    	cola=ifelse(p<(lambda/(lambda+mu)),cola+1,cola-1)
    	llegadas=ifelse(p<(lambda/(lambda+mu)),llegadas+1,llegadas+0)
    	servicios=ifelse(p<(lambda/(lambda+mu)),servicios+0,servicios+1)
    	cat("Cola actual:",cola,"\n")   		
} 
else 
{
    tiempo_exp = rexp(1,lambda)
    cat("Tiempo que ocurre el evento:",tiempo_exp,"\n")
    total_cola[numero_de_evento] = cola
    cola = 1
    llegadas= llegadas + 1
    cat("Cola actual:",cola,"\n")      	
}    
tiempo_actual = tiempo_actual+tiempo_exp
cat("Tiempo transcurrido:",tiempo_actual,"\n")
store = store+tiempo_exp*total_cola[numero_de_evento]
cat("\n")
}
cat("Total de llegadas:",llegadas,"\n")
cat("Total de servicios completados:",servicios,"\n")
cat("Longitud promedio de linea:",store/tiempo_actual,"\n")
}
