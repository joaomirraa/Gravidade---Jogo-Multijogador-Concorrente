import java.util.Map;
import java.util.concurrent.locks.*;

public class GameState {
    /*
     * Since GameState will be continuously updated by the server and read by the client, 
     * we need to implement a ReadWriteLock to ensure that the data is consistent.
     */
    
    public ReadWriteLock l = new ReentrantReadWriteLock(); 

    private int index; // Just to know what my player index is
    public float posX, posY, boost; // My position n stuff
    public Map<String, float[]>   enemies;
    public Map<String, float[]> planets;
    public boolean countdown;
    

    public GameState(float posX, float posY,float boost, String[] enemyIndex, float[] enemies, 
                    String[] planetIndexes, float[] planets, boolean countdown){
        this.posX = posX;
        this.posY = posY;
        this.boost = boost;
        for(int i=0; i < enemyIndex.length; i++){
            this.enemies.put(enemyIndex[i], new float[]{enemies[i*2], enemies[i*2 + 1]});
        }
        for(int i=0; i < planetIndexes.length; i++){
            this.planets.put(planetIndexes[i], new float[]{planets[i*2], planets[i*2 + 1]});
        }
        this.countdown = countdown;
    }
    
    public void setPos(float posX, float posY){
        this.posX = posX;
        this.posY = posY;
    }

    public void setEnemyPos(float x, float y, String i) {
        this.enemies.put(i, new float[]{x, y});
    }

    public void setPlanetPos(float x, float y, String i) {
        this.planets.put(i, new float[]{x, y});
    }

    public void setCountdown(boolean countdown){
        this.countdown = countdown;
    }

    public void setIndex(int index){
        this.index = index;
    }

}
