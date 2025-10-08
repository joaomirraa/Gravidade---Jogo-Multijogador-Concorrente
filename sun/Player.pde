class Player {
  PVector pos;
  PVector velocity;
  PImage player;
  
  Player(PVector pos) {
    this.pos = pos;
    String filename = "player.png";
    this.player = loadImage(filename);
  }
  
  void drawPlayer(){
    pushMatrix();
    translate(pos.x,pos.y);
    scale(0.2f);
    image(this.player, 0, 0);
    popMatrix();
  }
}
