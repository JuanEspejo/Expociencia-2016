markov <- function(X, T=10) 
{
      proceso <- matrix(,T,3)
      proceso[1,] <- X
      for (t in 2:T)
      {
            # calculando la longitud del arreglo X 
            L <- length(X) 
            
            # calculando las probabilidades de salida
            probabilidades <- X/sum(X) 
            
            # obteniendo la posicion de donde sale la particula
            i_salida <- sample(L, 1, prob = probabilidades) 
            
            # obteniendo la posicion a donde entra la particula
            i_entrada <- sample(L-1, 1) 
            if(i_entrada >= i_salida) i_entrada = i_entrada + 1 
            
            # actualizando estado
            X[i_salida] = X[i_salida] - 1
            X[i_entrada] = X[i_entrada] + 1      
            
            # registrando el nuevo estado
            proceso[t,] <- X
      }
      #retornando todos los estados registrados
      return(proceso)
}