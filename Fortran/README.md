```bash
apt-get update; 
apt-get upgrade -y; 
apt install -y gfortran
gfortran -o board main.f95
```

```bash
*Initialize file*
./board
```
![Fortran run](img/Fortraninit.png)

```bash
*Palyer move*
 P1 move? 
1
```
![Fortran run](img/Fortranplayermove.png)

## Run Fortran
![Fortran run](img/Correctrun.gif)