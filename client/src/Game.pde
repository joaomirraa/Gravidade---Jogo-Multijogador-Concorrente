import controlP5.*;
ControlP5 cp5;
Textfield usernameField, passwordField, lobbyNameField;


private static final int CREATE_ACCOUNT = 1;
private static final int LOGIN_ACCOUNT = 2;
private static final int LOGOUT_ACCOUNT = 3;
private static final int JOIN_ROOM = 4;
private static final int LEAVE_ROOM = 5;
private static final int CHANGE_NAME = 6;
private static final int CHANGE_PASS = 7;
private static final int REMOVE_ACCOUNT = 8;
private static final int CREATE_ROOM = 9;
private static final int LIST_ROOMS = 10;
private static final int UP_KEY = 11;
private static final int RIGHT_KEY = 12;
private static final int LEFT_KEY = 13;
private static final int CHAT_MESSAGE = 14;
private static final int LEAVE_CHAT = 15;
private static final int RANKING = 16;

enum State {
    MENU,
    LOGIN,
    REGISTER,
    PLAY,
    LOBBY,
    ROOM_CREATION,
    ROOM,
    GAME,
    LOADING
}

/* SANDBOX CLIENT WITH NO SERVER COMMUNICATION FOR GRAPHICS / MENU FLOW TESTING */

boolean isLoggedIn = false;
TCP tcp;
GameState gameState;
Worker[] workers;
State state;
Menu startMenu, loginMenu, registerMenu, playMenu, lobbyMenu, roomCreationMenu;
Lobby roomLobby;
float sun_radius = 200f;
ArrayList<PShape> rings = new ArrayList<PShape>();
Animation sun;
Player me; // My player :)
ArrayList<Player> players; // Other players
HashMap<Character, Boolean> keyMap = new HashMap<>();
PFont nightcore;
ArrayList<Planet> planets = new ArrayList<Planet>();
int counter; // 60 FPS, 5 seconds total
String username;
// Loading variables
PFont campus;
int points;
float i, m, n, p, s, t, x, y;
String success = "success";
String errorMsg="";

PImage backgroundImg; //!!!!!!!!!!!!!
PImage backgroundGameImg; //!!!!!!!!!!!!!
PImage starsImg; //!!!!!!!!!!!!!
PImage starsBluredImg; //!!!!!!!!!!!!!
PImage starsRedImg; //!!!!!!!!!!!!!
PImage starsRedBluredImg; //!!!!!!!!!!!!!
float translateStars = 0; //!!!!!!!!!!!!!
float translateStarsBlured = 0; //!!!!!!!!!!!!!

boolean gameStarted = false;
 

void setup() {
  try {
    tcp = new TCP("localhost", 12345);
    this.tcp.start();
  } catch (Exception e) {
    e.printStackTrace();
    println("Failed");
    exit();
    return;
  }
  fullScreen(P3D); // Set to FullScreen by default, not sure if this can be changed in settings after setup
  cp5 = new ControlP5(this); // Initialize controlP5 for textboxes and user input
  // Get fonts 
  nightcore = createFont("fonts/Satoshi-Variable.ttf", 100);
  campus = createFont("fonts/Satoshi-Variable.ttf", 50);
  // Initialize menus 
  startMenu = new Menu();
  initializeStartMenu();
  loginMenu = new Menu();
  initializeLoginMenu();
  registerMenu = new Menu();
  initializeRegisterMenu();
  playMenu = new Menu();
  initializePlayMenu();
  roomCreationMenu = new Menu();
  initializeRoomCreationMenu();
  roomLobby = new Lobby();
  //println(displayWidth, displayHeight);
  backgroundImg = loadImage("background/bg.png"); //!!!!!!!!!!!!!!!!!!
  backgroundGameImg = loadImage("background/bg_red.png");
  starsImg = loadImage("background/stars.png"); //!!!!!!!!!!!!!!!!!!
  starsBluredImg = loadImage("background/stars_blured.png"); //!!!!!!!!!!!!!!!!!!
  backgroundImg.resize(displayWidth, displayHeight); //!!!!!!!!!!!!!!!!!!
  backgroundGameImg.resize(displayWidth, displayHeight); //!!!!!!!!!!!!!!!!!!
  starsImg.resize(displayWidth, displayHeight); //!!!!!!!!!!!!!!!!!!
  starsBluredImg.resize(displayWidth, displayHeight);
  starsRedImg = loadImage("background/red_stars.png"); //!!!!!!!!!!!!!!!!!!
  starsRedBluredImg = loadImage("background/red_stars_blured.png"); //!!!!!!!!!!!!!!!!!!
  starsRedImg.resize(displayWidth, displayHeight); //!!!!!!!!!!!!!!!!!!
  starsRedBluredImg.resize(displayWidth, displayHeight);
  me = new Player();
  
  // State starts at MENU screen
  state = State.MENU;  
  
  // Create a new Animation of the Sun
  sun = new Animation("sun",91);
  // Create a new instance of Player -- TODO - Maybe delay this until player actually gets in the game ? Not sure if it makes a difference
  
  // Add players -- TODO
  //Planet planet1 = new Planet("planet1", 1);
  //planets.add(planet1);
  
  // Initialize the keyMap for player input -- this probably will not be used
  keyMap.put('w', false); // W -> false (not pressed)
  keyMap.put('a', false); // A -> false (not pressed)
  keyMap.put('s', false); // S -> false (not pressed)
  keyMap.put('d', false); // D -> false (not pressed)
  
  smooth(8);
  
  /*
  Initialize universe scenery contained within the rings array
  */
  /*
  float outerRad = 2000f, increment = 100f;
  for(int ringIndex = 0 ; ringIndex < 50 ; ringIndex++){
    PShape ring = createShape();
    ring.setStrokeWeight(random(1f,8f));
    ring.beginShape(POINTS);
    ring.stroke(random(170,255), random(170,255), random(170,255));
    for(int starIndex = 0 ; starIndex < 100 ; starIndex++){
      float a = random(0f, 1f) * TWO_PI;
      float r = sqrt(random(sq(sun_radius)+100, sq(outerRad)*10));
      ring.vertex(r * cos(a), r* sin(a),  60 * random(-increment, increment));
    }
    ring.endShape();
    rings.add(ring);
  }*/
  
  
  ellipseMode(RADIUS);
  frameRate(60);
}
void initializeWorkers() {
  this.workers = new Worker[9]; // 4 players - 4 planets - 1 Game worker
  this.workers[0] = new PosWorker(this.tcp, this.gameState, "1", "");
  this.workers[0].start();
  this.workers[1] = new PosWorker(this.tcp, this.gameState, "2", "");
  this.workers[1].start();
  this.workers[2] = new PosWorker(this.tcp, this.gameState, "3", "");
  this.workers[2].start();
  this.workers[3] = new PosWorker(this.tcp, this.gameState, "4", "");
  this.workers[3].start();
  this.workers[4] = new PlanetWorker(this.tcp, this.gameState, "1");
  this.workers[4].start();
  this.workers[5] = new PlanetWorker(this.tcp, this.gameState, "2");
  this.workers[5].start();
  this.workers[6] = new PlanetWorker(this.tcp, this.gameState, "3");
  this.workers[6].start();
  this.workers[7] = new PlanetWorker(this.tcp, this.gameState, "4");
  this.workers[7].start();
  this.workers[8] = new GameWorker(this.tcp, this.gameState);
  this.workers[8].start();
  
}

void initializeRoomCreationMenu() {
  Button createBtn = new Button("create", displayWidth/2 - 1263/2 - 100, displayHeight/2 + 385/2); 
  Button exitBtn = new Button("exit", displayWidth/2 + 100 , displayHeight/2 + 385/2);
  roomCreationMenu.addButton(createBtn);
  roomCreationMenu.addButton(exitBtn);
  
}


void initializeStartMenu() {
  Button loginBtn = new Button("login", displayWidth/2 - 1263/4, 75);
  Button registerBtn = new Button("register", displayWidth/2 - 1263/4, 75 + 50 + 385/2);
  Button settingsBtn = new Button("settings", displayWidth/2 - 1263/4, 75 + (50 + 385/2)*2);
  Button exitBtn = new Button("exit", displayWidth/2 - 1263/4, 75 + (50 + 385/2)*3);
  startMenu.addButton(loginBtn);
  startMenu.addButton(registerBtn);
  startMenu.addButton(settingsBtn);
  startMenu.addButton(exitBtn);
}

void initializeLoginMenu() {
  Button loginBtn = new Button("login", displayWidth/2 - 1263/2 - 100, displayHeight/2 + 385/2);
  Button backBtn = new Button("back", displayWidth/2 + 100, displayHeight/2 + 385/2);
  loginMenu.addButton(loginBtn);
  loginMenu.addButton(backBtn);
  // Create Username Textfield
  usernameField = cp5.addTextfield("Username")
                     .setPosition((displayWidth/2 - 200),(displayHeight/2) - 250)
                     .setSize(400,75)
                     .setColor(255)
                     .setColorBackground(255)
                     .setFont(createFont("arial",42))
                     .setFocus(true)
                     .setVisible(false);

  // Create Password Textfield
  passwordField = cp5.addTextfield("Password")
                     .setPosition((displayWidth/2 - 200),(displayHeight/2) - 100)
                     .setSize(400, 75)
                     .setPasswordMode(true)
                     .setColor(255)
                     .setColorBackground(255)
                     .setFont(createFont("arial",42))
                     .setVisible(false);
  
  lobbyNameField = cp5.addTextfield("Lobby name")
                      .setPosition((displayWidth/2 - 200),(displayHeight/2) - 250)
                      .setSize(400,75)
                      .setColor(255)
                      .setColorBackground(255)
                      .setFont(createFont("arial",42))
                      .setVisible(false);
}


void initializeRegisterMenu() {
  Button registerBtn = new Button("register", displayWidth/2  - 1263/2 - 100, displayHeight/2 + 385/2);
  Button backBtn = new Button("back", displayWidth/2 + 100, displayHeight/2 + 385/2);
  registerMenu.addButton(registerBtn);
  registerMenu.addButton(backBtn);
}

void initializePlayMenu() {
  Button joinBtn = new Button("join", displayWidth/2 - 1263/4, 75);
  Button createBtn = new Button("create", displayWidth/2 - 1263/4, 75 + 50 + 385/2);
  Button logoutBtn = new Button("logout", displayWidth/2 - 1263/4, 75 + (50 + 385/2)*2);
  Button exitBtn = new Button("exit", displayWidth/2 - 1263/4, 75 + (50 + 385/2)*3);
  playMenu.addButton(joinBtn);
  playMenu.addButton(createBtn);
  playMenu.addButton(logoutBtn);
  playMenu.addButton(exitBtn);
  
}

  // Key pressed method
  void keyPressed() {
    switch(state) {
      case GAME:
        if (keyMap.containsKey(key)) {
          keyMap.put(key, true); // Update key state to pressed
        }
        break;
      case LOGIN:
        if (keyCode == TAB) {
          if (usernameField.isFocus()) {
            usernameField.setFocus(false);
            passwordField.setFocus(true);
          }
          else {
            usernameField.setFocus(true);
            passwordField.setFocus(false);
          }
        }
        break;
      case REGISTER:
        if (keyCode == TAB) {
          if (usernameField.isFocus()) {
            usernameField.setFocus(false);
            passwordField.setFocus(true);
          }
          else {
            usernameField.setFocus(true);
            passwordField.setFocus(false);
          }
        }
        break;
      default:
        break;
    }
  }

  // Key released method
  void keyReleased() {
    switch(state) {
      case GAME:
        if (keyMap.containsKey(key)) {
          keyMap.put(key, false); // Update key state to released
        }
      default:
        break;
    }
  }
  
void mousePressed() {
  switch(state) {
    case MENU:
      checkStartMenuButtons();
      break;
    case LOGIN:
      checkLoginMenuButtons();
      break;
    case REGISTER:
      checkRegisterMenuButtons();
      break;
    case PLAY:
      checkPlayMenuButtons();
      break;
    case ROOM_CREATION:
      checkRoomCreationButtons();
      break;
    case LOBBY:
      roomLobby.getRoomByLocation();
      checkLobbyButtons();
      break;
    case GAME:
      me = new Player();
    default:
      break;
  }
}
  
  
void toggleUserFields() {
  if(usernameField.isVisible()) usernameField.setVisible(false);
  else usernameField.setVisible(true);
  if(passwordField.isVisible()) passwordField.setVisible(false); 
  else passwordField.setVisible(true);
  usernameField.setText("");
  passwordField.setText("");
}

void checkRoomCreationButtons() {
  for (Button b: roomCreationMenu.getButtons()) {
    if (b.isClicked()) {
      //println("Clicked button");
      switch (b.getName()) {
        case "create":
          if (authCreateRoom()) {
            state = State.LOBBY;
            roomLobby.updateLobby(lobbyNameField.setVisible(false).getText());
          }
          
          break;
        case "exit":
          lobbyNameField.setVisible(false);
          state = State.PLAY;
          break;
      }
      break;
    }
  }
}

void checkLobbyButtons() {
  for (Button b: roomLobby.getButtons()) {
    if (b.isClicked()) {
      switch(b.getName()) {
        case "join":
          if (roomLobby.isJoined()) {
             errorMsg = "Leave your room first";
          }
          else {
            if (authJoinRoom()){
                roomLobby.updateJoined();
                roomLobby.setJoinedRoom();
            }
          }
          break;
        case "leave":
          if (authLeaveRoom()) {
            roomLobby.leaveRoom();
          }
          break;
        case "exit":
          // Commented because Leave_room is currently not working properly
          if (roomLobby.isJoined()) { 
            if (authLeaveRoom()) {
              roomLobby.leaveRoom();
              state = State.PLAY;
            }
            else {
              println("Failed to leave room => " + roomLobby.getJoinedRoom());
            }
          } else {
            state = State.PLAY;
          }
          break;
        case "refresh":
          authBrowseRooms();
          break;
        default:
          break;
      }
    }
  }
}


void checkStartMenuButtons() {
  for (Button b: startMenu.getButtons()) {
    if (b.isClicked()) {
      //println("Clicked button");
      switch (b.getName()) {
        case "login":
          //delay(10);
          state = State.LOGIN;
          toggleUserFields();
          //println("Got the login\n");
          break;
        case "register":
          //delay(10);
          state = State.REGISTER;
          toggleUserFields();
          break;
        case "settings":
          break;
        case "exit":
          exit();
          return;
      }
      break;
    }
  }
}

boolean authBrowseRooms() {
  String res = "";
  try {
    this.tcp.send(LIST_ROOMS,"");
    res = this.tcp.receive("res");
    println(res);
  } catch (IOException | InterruptedException e) {
    println("Failed");
  }
  if (!res.equals("")) {
    println(res);
    String[] temp = res.split("@@@");
    for(int i =0; i<temp.length;i++) {
      println(temp[i]);
    }
    roomLobby.refresh(temp);  
  }
  else {
    roomLobby.refresh(new String[0]);
    return true;
  }
  
  return res.equals("");  // just in case
}

boolean authLeaveRoom() {
  if (!roomLobby.isJoined()) {
    return false;
  }

  String res = "";
  String room = roomLobby.getJoinedRoom();
  if (room.equals("")) return false;
  
  try {
    this.tcp.send(LEAVE_ROOM, "");
    res = this.tcp.receive("res");
  } catch (Exception e){
    println("Failed to leave room");
    e.printStackTrace();
    return false;
  }
  if (!res.equals(success)) {
    errorMsg = res;
  }
  return res.equals(success);
}  

boolean authCreateRoom() {
  String res = "";
  String lobbyName = lobbyNameField.getText();
  try {
    this.tcp.send(CREATE_ROOM,lobbyName);
    res = this.tcp.receive("res");
  } catch(Exception e) {
    println("Failed room creation due to exception");
  }
  println(res);
  return res.equals(success);
  
}

boolean authJoinRoom() {
  String res = "";
  String room = roomLobby.getSelectedRoom();
  if (room.equals("")) {
    println("No room selected.");
    return false;
  }
  try {
    this.tcp.send(JOIN_ROOM,room);
    res = this.tcp.receive("res");
  } catch (Exception e) {
    System.out.println("Failed message.");
    e.printStackTrace();
    return false;
  }
  return res.equals(success);
}  
    



boolean authLogin() {

  String res = "";
  String username = usernameField.getText();
  String password = passwordField.getText();
  try { 
    this.tcp.send(LOGIN_ACCOUNT, username+ "@@@" + password);
    res = this.tcp.receive("res");
  } catch(Exception e) {
    println("Failed auth");
  }
  println(res);
  if(!res.equals(success)) {
    errorMsg = res;
  }
  return res.equals(success);
}

boolean authRegister() {
  String username = usernameField.getText();
  String password = passwordField.getText();
  try {
    this.tcp.send(CREATE_ACCOUNT, username + "@@@" + password);
    String res = this.tcp.receive("res");
  }  catch(Exception e) {
    println("Failed.");
    e.printStackTrace();
    return false;
  }
  return authLogin(); // Immediately login after registration*/
}

boolean authLogoutUser() {
  String res = "";
  try {
    this.tcp.send(LOGOUT_ACCOUNT, "");
    res = this.tcp.receive("res");
  } catch (Exception e) {
    println("Failed");
    return false;
  }
  return res.equals(success);
}

void checkLoginMenuButtons() {
  for (Button b: loginMenu.getButtons()) {
    if (b.isClicked()) {
      switch(b.getName()) {
        case "login":
          if (authLogin()) {
            this.username = usernameField.getText();
            toggleUserFields();
            errorMsg = "";
            isLoggedIn = true;
            state = State.PLAY;
          } else {
            isLoggedIn = false;
          }
          
          break;
        case "back":
          toggleUserFields();
          state = State.MENU;
          break;
        default:
          break;
      }
    }
  }
}

void checkPlayMenuButtons() {
  for (Button b: playMenu.getButtons()) {
    if (b.isClicked()) {
      switch(b.getName()) {
        case "join":
          if(authBrowseRooms()) {
            state = State.LOBBY;
          }
          state = State.LOBBY;
          break;
        case "create":
          state = State.ROOM_CREATION;
          lobbyNameField.setVisible(true);
          break;
        case "logout":
          if (authLogoutUser()) {
            state = State.MENU;
          }
          break;
        case "exit":
          exit();
          return;
        default:
          break;
      }
    }
  }
}
          
void checkRegisterMenuButtons() {
  for (Button b: registerMenu.getButtons()) {
    if (b.isClicked()) {
      switch(b.getName()) {
        case "register":
          if (authRegister()) {
            toggleUserFields();
            state = State.PLAY;
          }
          break;
        case "back":
          toggleUserFields();
          //delay(10);
          state = State.MENU;
          break;
        default:
          break;
      }
    }
  }
}

  
void drawRings() {
  pushMatrix();
  translate(displayWidth/2, displayHeight/2);
  rotateZ(PI/1000 * frameCount);
  rotateY(-PI/500 * frameCount);
  rotateX(PI/1000 * frameCount);
  for(PShape r : rings){
    shape(r);    
  }
  popMatrix();
}


void drawSun() {
  fill(random(190,170), 64, 37);
  sun.display();
}


void drawPlayer() {
  if (me != null) {
    if (me.getBoost() > 0) me.applyGravity(keyMap);
    else me.outOfBoost();
    if (me.checkCollisions()) {
        me = null;
        textFont(nightcore);
        text("YOU LOST", 650, displayHeight/2 - 200); 
    }
    else {
      drawPlayerBoost();
      me.display();
    }
  }
  
  /*
  
  for(Player player: players) {
    if (player != null) {
      
      if (player.getBoost() > 0) player.applyGravity(keyMap);
      else {
        player.outOfBoost();
      }
      if (player.checkCollisions()) {
        player = null;
        textFont(nightcore);
        text("YOU LOST", 650, displayHeight/2 - 200); 
      }
      else {
        drawPlayerBoost();
        player.display();
      }
    }*/
    
    
    else {
  
        textFont(nightcore);
        text("YOU LOST", 650, displayHeight/2-200); 
    }
  }


void drawPlanets() {
  for(Planet p: planets) {
    p.display();
  }
}

void drawPlayerBoost() {
  int boost = (int)me.getBoost();
  textFont(campus);
  textSize(36);
  fill(255);
  if (boost == 0) fill(100);
  text("BOOST:" + boost,displayWidth-200,displayHeight-100);
}

void drawMargins() { // Unused, they looked ugly and out of place
  strokeWeight(3f);
  stroke(255);
  line(0,0,displayWidth,0);                           // horizontal top line
  line(displayWidth,0,displayWidth,displayHeight);    // right vertical line
  line(0,0,0,displayHeight);                          // left vertical line
  line(0,displayHeight, displayWidth, displayHeight); // horizontal bottom line
}



void loadingScreen() {
       textFont(campus);
      if (points < 50) text("LOADING", 580, displayHeight-100);
      else if (points < 100) text("LOADING.", 580, displayHeight-100);
      else if (points < 150) text("LOADING..", 580, displayHeight-100);
      else text("LOADING...", 580, displayHeight-100);
      
      points = (points+1) % 200;
      pushMatrix();
      noStroke();
      fill(255);
      t+=.2;
      translate(850,450);
      for (i=2e3; i>0; i--) {
        p=i%2==0 ? 0 : 1;
        m=t/cos(t/i)+p*(t/2+i%t);
        n=t/9+i*i;
        x=m*2*sin(n)*cos((p==0 ? 1 : 0)*i/t);
        y=m*2*cos(n+p*2);
        s=5-cos(n)*3;
        rect(x, y, s, s);
      }
      popMatrix();
}

void drawBackground(){ //!!!!!!!!!!!!!!!!!!
  if (state == State.GAME){
    image(backgroundGameImg, 0, 0); 
  } else{
    image(backgroundImg, 0, 0); 
  }
  
  pushMatrix();
  translate(translateStarsBlured+displayWidth,0);
  if (state == State.GAME){
    image(starsRedBluredImg, 0, 0);
  } else{
    image(starsBluredImg, 0, 0);
  }
  popMatrix();
  
  pushMatrix();
  translate(translateStarsBlured,0);
  if (state == State.GAME){
    tint(255, 127);
    image(starsRedBluredImg, 0, 0);
    tint(255, 255);
  } else{
    image(starsBluredImg, 0, 0);
  }
  popMatrix();
  
  pushMatrix();
  translate(translateStars+displayWidth,0);
  if (state == State.GAME){
    image(starsRedImg, 0, 0);
  } else{
    image(starsImg, 0, 0);
  }
  popMatrix();
  
  pushMatrix();
  translate(translateStars,0);
  if (state == State.GAME){
    image(starsRedImg, 0, 0);
  } else{
    image(starsImg, 0, 0);
  }
  popMatrix();
  if (translateStars <= -displayWidth){
    translateStars=translateStars + displayWidth;
  }
  if (translateStarsBlured <= -displayWidth){
    translateStarsBlured=translateStarsBlured + displayWidth;
  }
  translateStars = translateStars - 1 * 0.125;
  translateStarsBlured = translateStarsBlured - 1 * 0.0625;
 
}

void draw() {
  
  drawBackground(); //!!!!!!!!!!!!!!!!!!
  //drawRings();
  switch(state) {
    case MENU:
      startMenu.drawMenu();
      break;
    case LOGIN:
      loginMenu.drawMenu();
      
      if (!isLoggedIn) {
        text(errorMsg, displayWidth/2-200, 100);
      }
      break;
    case REGISTER:
      registerMenu.drawMenu();
      break;
    case PLAY:
      playMenu.drawMenu();
      break;
    case LOADING:
      loadingScreen();
      if (frameCount > counter){
        state = State.GAME;
        delay(100);
      }
      break;
    case LOBBY:
      String res;
      try{
        res = this.tcp.waiting("res");
        
        if (res.equals("")){
          roomLobby.drawLobby();
        } else if (res.equals("countdown_started")) {
          
        } else if (res.equals("enter_game")) {
           counter = frameCount + 2*60; // 2 seconds
           state = State.LOADING;
        }
      } catch (Exception e) {
        println("Failed lobby wait.");
      }
      roomLobby.drawLobby();
      break;
    case ROOM_CREATION:
      roomCreationMenu.drawMenu();
      break;
    case ROOM:

      state = State.LOADING;
      break;
    case GAME:
      //drawMargins();
      //drawRings();
      drawPlayer();
      //drawPlanets();
      drawSun();
      break;
  
  }
  //println(frameRate);
}
