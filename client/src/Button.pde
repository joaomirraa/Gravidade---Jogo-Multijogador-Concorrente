class Button {
 PImage image, hover;
 float x,y;
int width, height;
 String name;
 
 Button(String name, float x, float y){ // Buttons should receive a name and a position
   this.name = name;
   this.image=loadImage("buttons/" + name + ".png"); 
   this.hover=loadImage("buttons/" + name + "_hover" + ".png");
   width = 1263/2;
   height = 385/2;
   this.image.resize(width, height);
   this.hover.resize(width, height);
   this.x = x;
   this.y = y;
 }
 
  Button(String name, float x, float y, int wid, int hei){ // Buttons should receive a name and a position
   this.name = name;
   this.image=loadImage("buttons/" + name + ".png"); 
   this.hover=loadImage("buttons/" + name + "_hover" + ".png");
   width = wid;
   height = hei;
   this.image.resize(width, height);
   this.hover.resize(width, height);
   this.x = x;
   this.y = y;
 }
 
 String getName() {
   return this.name;
 }
 

 boolean isClicked() {
   return (mouseX < x + width  && mouseX > x && mouseY < y + height && mouseY > y);
 }
 
 void drawBtn() {
   if (isClicked()) {
     
     image(this.hover, x, y);
   }
   else {
     image(this.image, x, y);
   }
 }
}
