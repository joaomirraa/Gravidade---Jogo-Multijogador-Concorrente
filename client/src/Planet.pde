class Planet {
  PImage[] images;
  int imageCount;
  int counter;
  float radius = 100f;
  ArrayList<PVector> trajectory = new ArrayList<PVector>();
    
  Planet(String model, int imageCount) {
    this.imageCount = imageCount;
    images = new PImage[imageCount];

    for (int i = 0; i < imageCount;i++){
      String filename = model + "_" + nf(i,5)+ ".png"; //!!!!!!!!!!!!

      images[i] = loadImage(filename);
      images[i].resize((int)radius,(int)radius);
    }
    
    generateBezierTrajectory();
  }
  
  void generateBezierTrajectory() {
    //boolean side = random(0,1) > 0.5; // false for left side, true for right side
    if (true ) {//!side) {
      float startingY = random(215, displayHeight);
      int steps = 500;
      for (int i = 0; i <= steps; i++) {
        PVector pos = new PVector();
        float t = i / float(steps);
        pos.x =  bezierPoint(0, displayWidth/10, displayWidth/1.2, displayWidth+100, t);
        pos.y = bezierPoint(startingY, 200, 300, displayHeight, t);
        trajectory.add(pos);
      }
    }
  }
 
  void display() {
    
    counter++;
    counter = counter % trajectory.size();
    PVector pos = trajectory.get(counter);    
    image(images[0],pos.x,pos.y);


  }
}
