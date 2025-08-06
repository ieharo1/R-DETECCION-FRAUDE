"use strict"
//Condicion IF
var dato1=20;
var dato2=30;
var dato3=40;
var dato4=2;

/*if(){

}*/ //Estructura de un if

if(dato1<dato2){
    console.log("dato1 es menor que dato2")
}

if(dato1>dato2){
    console.log("dato1 es mayor que dato2")
}
else{
    console.log("dato1 es menor que dato1")
}

if(dato2>dato4){
    console.log("dato 2 es mayor que el dato4")
}
else if(dato4<dato3){
    console.log("dato4 es menor que el dato3")
}
else{
    console.log("no entro a ningun condicional")
}

if(dato1<dato2 && dato1<dato3){
    console.log("dato1 es menor que dato2 y dato3")
}

//Condiciones anidadas
if(dato1>dato4){
    if(dato2>dato1){
        console.log("dato anidado:D")
    }
}

//Switch

var switch1=1;
switch (switch1) {
    case 1:
        console.log("caso 1 entro");
        break;
    case 2:
        console.log("caso 2 entro");
        break;
    default:
        break;
}
