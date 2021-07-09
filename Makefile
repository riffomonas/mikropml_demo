raw_data/baxter.% : 
	wget https://github.com/riffomonas/raw_data/archive/refs/tags/0.3.zip
	unzip 0.3.zip
	mv raw_data-0.3 raw_data
	rm 0.3.zip