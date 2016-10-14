function [F_Y,F_Yi,C] = FourierExp1D(Y,X,M,graf)

n = length(X); T = X(n)-X(1); w = 2*pi/T; dX = X(2)-X(1);

% CALCULO DE LOS COEFICIENTES DE FOURIER

for p = 1:(2*M+1)
    C_Sum_inf = 0; C_Sum_sup = 0;
    for j = 1:(n-1)
        C_Sum_inf = C_Sum_inf + Y(j)*exp(-1i*(p-M-1)*w*X(j))*dX;
        C_Sum_sup = C_Sum_sup + Y(j+1)*exp(-1i*(p-M-1)*w*X(j+1))*dX;
    end
C(p) = (1/T)*(C_Sum_inf+C_Sum_sup)/2; % Coeficiente de Fourier
end

% CALCULO DE Y DE FOURIER: F_Y

for j = 1:n
    Y_Fourier_Sum = 0;
    for p = 1:(2*M+1)
        Y_Fourier_Sum = Y_Fourier_Sum + C(p)*exp(1i*(p-M-1)*w*X(j));
    end
F_Y(j) = real(Y_Fourier_Sum);
F_Yi(j) = imag(Y_Fourier_Sum);
end

% CALCULO DEL COEFICIENTE DE CORRELACION NO LINEAL

% Ymean: Promedio de los valores de Y
Ymean = 0;
for j = 1:n
    Ymean = Ymean + Y(j);
end
Ymean = Ymean/n;

%  SSE: Suma de los errores al cuadrado
%  SST: Suma de las desviaciones de la media al cuadrado
SSE = 0; SST = 0;
for j = 1:n
    SSE = SSE + (Y(j)-F_Y(j))^2;
    SST = SST + (Y(j)-Ymean)^2;
end

% r: Coeficiente de correlacion no lineal
r = sqrt(1-SSE/SST)
r2 = 1-SSE/SST

% GRAFICA DE Y y DE F_Y

switch graf
    
    case 1 % Grafica Y y F_Y
        hold on
        title('Azul: Numérico.   Rojo: Fourier.')
        xlabel('Componente "X"')
        ylabel('Componente "Y"')
        plot(X,Y)       % Azul: Numerico
        plot(X,F_Y,'r') % Rojo: Fourier
    case 2 % Grafica F_Y
        hold on
        title('Rojo: Fourier')
        xlabel('Componente "X"')
        ylabel('Componente "Y"')
        plot(X,F_Y,'r') % Rojo: Fourier
    case 3 % Grafica F_Y - Y
        hold on
        title('F_Y - Y')
        xlabel('Componente "X"')
        ylabel('Componente "Y"')
        plot(X,F_Y - Y)
    case 4 % Grafica F_Yi
        hold on
        title('F_Yi (complejo)')
        xlabel('Componente "X"')
        ylabel('Componente "Y"')
        plot(X,F_Yi)
    
end

end