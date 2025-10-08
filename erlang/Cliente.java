Public classs Cliente{
	public static void main(String[] args){
		try {
			if(args.length < 2)
				System.exit(1);

			String host = args[0];
			int port = Integer.parseInt(args[1]);
			Socket s = new Socket(host,port);
			BufferedReader in = new BefferedReader (new InputStreamReader(s.getInpurStream()));
			PrintWriter out = new PrintWriter(s.getOutputStream());
			BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));

			new Thread(() -> {
				try{
					for(;;){
						String str = in.readLine();
						if(str == null) break;
						System.out.println(str);	
					}
					catch (IOException ignored){}
				}
			}).start();

			for(;;) {
				String str = stdin.readLine();
				if(str == null) break; 
				out.println(str);
				out.flush();
			}

			s.close(); 
		} catch(Exception ignored) {}
 	}
}