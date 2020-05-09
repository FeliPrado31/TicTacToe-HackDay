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
*Player move*
 P1 move? 
1
```
![Fortran run](img/Fortranplayermove.png)

```bash
*Player move*
 P2 move? 
5
```
![Fortran run](img/Fortranplayermove2.png)

## Run Fortran
![Fortran run](img/Correctrun.gif)