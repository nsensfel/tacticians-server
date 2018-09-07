# Tacticians Online - Server
This is the code for the back-end of Tacticians Online, an online
multiplayer turn-based strategy game.

## Getting Started

### Prerequisites
* Erlang
* GNU make
* Jiffy (JSON NIFs for Erlang)
* Tacticians Online - Data
* Yaws
* m4

### Installing
1. Go into the directory in which this repository was cloned.
2. Set the DATA\_DIR variable in `./Makefile` to match the "Tacticians Online -
   Data" directory.
3. Copy Yaws's `yaws_api.hrl` file to the `./include/` directory.
4. Run `$ make`.
5. Run `$ make run_db_node`, and then `$ make run_query_node` in parallel.
6. You should be done. If Erlang complains about node addresses, you might need
   to set the `ERL_NAME_VS_SNAME` variable in `./Makefile` to its other value.

## Screenshot (of the Client)
![Screenshot of a battle](https://noot-noot.org/to-2018-09-07.png)

## License
Apache License 2.0 (see `./LICENSE`)
