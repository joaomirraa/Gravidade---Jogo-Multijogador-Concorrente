class Player {
  PVector location;
  PVector velocity;
  PVector acceleration;
  // The player's maximum speed
  float topspeed;
  float radius = 25f; // default radius of a player 
  PVector sunPos = new PVector(displayWidth/2, displayHeight/2);
  float booster = 100f;
  PImage PlayerImg, ShadowImg;
  Player() {
    location = new PVector(200,200);
    velocity = new PVector(0,0);
    this.topspeed = 10;
    this.booster = 100f;
    PlayerImg = loadImage("/player/player.png");
    ShadowImg = loadImage("/player/player_shadow.png");
    PlayerImg.resize(50, 50);
    ShadowImg.resize(25, 50);
  }

  void outOfBoost() {
    PVector sun = new PVector(displayWidth/2, displayHeight/2);
      PVector acceleration = PVector.sub(sun, location);
      
      // All the following values were obtained by pure trial and error, no math was harmed in this experiment
      acceleration.setMag(0.1);
      velocity.add(acceleration);
      // Limit the velocity by topspeed
      velocity.limit(topspeed);
      // Location changes by velocity
      location.add(velocity);

  }
  
  void setPos(PVector pos) {
    this.location = pos;
  }
  
  void setVelocity(PVector vel) {
    this.velocity = vel;
  }
  
  void applyGravity(HashMap<Character, Boolean> keyMap) {
    PVector sun = new PVector(displayWidth/2, displayHeight/2);
    PVector acceleration = PVector.sub(sun, location);
    float alpha = 0;
      
      // All the following values were obtained by pure trial and error, no math was harmed in this experiment
    acceleration.setMag(0.1);

      // Modify acceleration based on user input
    PVector controlAcceleration = new PVector(0, 0);
    if (keyMap.get('w')) {
        controlAcceleration.add(0, -0.2); // Move up
        booster -= 0.5f;
    }
    if (keyMap.get('a')) {
        controlAcceleration.add(-0.2, 0); // Move left
        booster -= 0.5f;
    }
    if (keyMap.get('d')) {
        controlAcceleration.add(0.2, 0); // Move right
        booster -= 0.5f;
    }
    if (keyMap.get('s')) {
      controlAcceleration.add(0, 0.2); // Move right
      booster -= 0.5f;
    }

    // Combine acceleration vectors
    acceleration.add(controlAcceleration);
    // Velocity changes according to acceleration
    velocity.add(acceleration);
    // Limit the velocity by topspeed
    velocity.limit(topspeed);
    // Location changes by velocity
    location.add(velocity);
  }
  
  boolean checkCollisions() {
    float distance = PVector.dist(location, sunPos);
    if (distance < radius + 170) {
      return true;
    }
    return (location.x < 0 || location.x > displayWidth || location.y < 0 || location.y > displayHeight);  
  }
  
  float getBoost() {
    return this.booster;
  }
 
  
  void display() { //!!!!!!!!!!!!!
    PVector v1 = new PVector(300, displayHeight/2) ;

    image(PlayerImg, location.x, location.y);
    pushMatrix();
    translate(location.x+25, location.y+25);
    PVector direction = PVector.sub(sunPos, this.location);
    rotate(PI+atan2(direction.y, direction.x));
    image(ShadowImg, 0, -25); // rotate in relation of sun position
    popMatrix();
  }
}
