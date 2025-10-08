class Menu {
  ArrayList<Button> btns;
  
  Menu() {
    this.btns = new ArrayList<Button>();
  }
  
  void addButton(Button btn) {
    this.btns.add(btn);
  }
  
  ArrayList<Button> getButtons() {
    return this.btns;
  }
  void drawMenu() {
    for(Button btn : btns) {
      btn.drawBtn();
    }
  }
}
