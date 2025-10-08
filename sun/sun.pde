int margin = 3;
float sun_radius = 135f;
float rotation_factor = 8000;
ArrayList<PShape> rings = new ArrayList<PShape>();
Animation sun;
//PVector playerPos = new PVector(300,0);
//Player p1;
 
int num = 60;
//float mx[] = new float[num];
//float my[] = new float[num];

void setup() {
  fullScreen(P3D);
  //size(1300,900,P3D);
  //sphereDetail(60);
  //colorMode(HSB, 360, 100, 100, 100);
  //p1 = new Player(playerPos);
  sun = new Animation(60);
  //noSmooth();
  smooth(8);
  //blendMode(BLEND);
  float outerRad = 2000f, increment = 100f;
  for(int ringIndex = 0 ; ringIndex < 50 ; ringIndex++){
    PShape ring = createShape();
    ring.setStrokeWeight(random(1f,8f));
    ring.beginShape(POINTS);
    ring.stroke(random(180,255), random(180,255), random(180,255));
    for(int starIndex = 0 ; starIndex < 100 ; starIndex++){
      float a = random(0f, 1f) * TWO_PI;
      float r = sqrt(random(sq(sun_radius)+100, sq(outerRad)*10));
      ring.vertex(r * cos(a), r* sin(a),  60 * random(-increment, increment));
    }
    ring.endShape();
    rings.add(ring);
  }
  
  
  ellipseMode(RADIUS);
  frameRate(60);
}


void draw() {
  //float surprise = 0.2f / 200 * frameCount;
  background(15);
  
  //pushMatrix();
  translate(displayWidth/2, displayHeight/2,0);
  
  pushMatrix();
  rotateZ(PI/3000 * frameCount);
  rotateY(-PI/ 500 * frameCount);
  rotateX(PI/ 3000 * frameCount);
  
  for(PShape r : rings){
    shape(r);    
  }
  
  popMatrix();
  //p1.drawPlayer();
  fill(random(190,170), 64, 37);
  noStroke();
  pushMatrix();
  rotateZ(-PI/1000 * frameCount);
  translate(0,0,random(-1f,1f));
  //strokeWeight(2f);
  //sphere(sun_radius);
  //scale(0.5f + surprise);
  //stroke(170,64,38); // Outline of circle behind the sun
  scale(1.5f);
  circle(0,0,sun_radius);
  sun.display();
  popMatrix();
  
  
  //popMatrix();
 

  println(frameRate);
}
