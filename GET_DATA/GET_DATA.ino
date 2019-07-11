#define pin_sensor A5
float spd_cte = 3600;
char input;
float adc;
float voltaje;
float value; 
int contador; 



void setup() {
    Serial.begin(9600); 
    pinMode(pin_sensor, INPUT);
}

void loop() {
  contador=1;
  Serial.println("Pulsa 1 para comenzar la toma de datos");
  while (!Serial.available()){
    //Do Absolutely Nothing until something is received over the serial port
  }

 
  if(Serial.available()>0){
     input= Serial.read();
      if(input== '1'){
        while(contador < 600){
    //MITICA_LINEA ANALOGREAD
        adc = analogRead(pin_sensor);//lectura analógica anemómetro
        voltaje = adc * 5 / 1023; //conversión lectura voltaje
        value = voltaje * spd_cte; 
        Serial.println(value);
        contador++;
        delay(100);
    
    }     
    input=0;
    }
  
}

}
