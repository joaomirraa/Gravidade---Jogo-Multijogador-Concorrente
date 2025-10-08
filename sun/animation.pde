class Animation {
  PImage[] images;
  int imageCount;
  int frame;
  int frame_counter;
  Animation(int count) {
    imageCount = count;
    images = new PImage[imageCount];
    

    for (int i = 0; i < imageCount; i++) {
      String filename = "sun_" + nf(i,2) + ".png";
      images[i] = loadImage(filename);
    }
  }
  
  void display() {
    //frame_counter = (frame_counter+1) % imageCount;
    //if (frame_counter % 2 == 0) frame = (frame+1) % imageCount;
    frame = (frame+1) % imageCount;
    image(images[frame], -215,-215);
  }
}
