
import java.io.IOException;


public class Worker extends Thread {
    public TCP tcp;
    public GameState gameState;
    public Worker(TCP tcp, GameState gameState) {
        this.tcp = tcp;
        this.gameState = gameState;
    }
}

class PosWorker extends Worker {
    public String username;
    private final String index;

    public PosWorker(TCP tcp, GameState gameState, String index, String username) {
        super(tcp, gameState);
        this.index = index;
        this.username = username;
    }
    
    
    public void setUsername(String user) {
      this.username = user;
    }

    public void go() throws IOException, InterruptedException {
        String res;
        try {
            while((res = this.tcp.receive("pos"+ this.index)) != null) {
                String[] pos = res.split("@@@");
                this.gameState.l.readLock().lock();
                this.gameState.setEnemyPos(Float.parseFloat(pos[0]), Float.parseFloat(pos[1]), this.index);
            }
        } finally {
            this.gameState.l.readLock().unlock();
        }
    }
}

class PlanetWorker extends Worker {
    private final String index;

    public PlanetWorker(TCP tcp, GameState gameState, String index) {
        super(tcp, gameState);
        this.index = index;
    }

    public void go() throws IOException, InterruptedException {
        String res;
        try {
            while((res = this.tcp.receive("p"+ this.index)) != null) {
                String[] pos = res.split("@@@");
                this.gameState.l.readLock().lock();
                this.gameState.setPlanetPos(Float.parseFloat(pos[0]), Float.parseFloat(pos[1]), this.index);
            }
        } finally {
            this.gameState.l.readLock().unlock();
        }
    }
}

class GameWorker extends Worker {
    public GameWorker(TCP tcp, GameState gameState) {
        super(tcp, gameState);
    }

    public void go() throws IOException, InterruptedException {
        String res;
        try {
            while((res = this.tcp.receive("game")) != null) {
                switch (res) {
                    case "countdown_start":
                        this.gameState.l.readLock().lock();
                        this.gameState.setCountdown(true);
                        break;
                    case "countdown_end":
                        this.gameState.l.readLock().lock();
                        this.gameState.setCountdown(false);
                        break;
                    case "player_index":
                        this.gameState.l.readLock().lock();
                        this.gameState.setIndex(Integer.parseInt(res.split("@@@")[1]));
                        break;
                    default:
                        break;
                    
                }
            }
        } finally {
            this.gameState.l.readLock().unlock();
        }
    }
}
