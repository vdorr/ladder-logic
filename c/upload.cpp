
#include <cstdio>
#include <unistd.h>

#include <modbus/modbus.h>

/*
gcc upload.cpp -lmodbus
*/

int main() {
	modbus_t *ctx = modbus_new_rtu("/dev/ttyACM0", 9600, 'N', 8, 1);

	printf("%s:%i\n", __FUNCTION__, __LINE__);

	if ( !ctx ) {
	    fprintf(stderr, "Unable to create the libmodbus context\n");
	    return -1;
	}

	int ret;

	ret = modbus_set_slave(ctx, 1);
	printf("%s:%i ret:%i\n", __FUNCTION__, __LINE__, ret);

	ret = modbus_connect(ctx);
	printf("%s:%i ret:%i\n", __FUNCTION__, __LINE__, ret);

	for (;;) {

		ret = modbus_write_bit(ctx, 13, 1);
		printf("%s:%i ret:%i\n", __FUNCTION__, __LINE__, ret);
		sleep(1);

		ret = modbus_write_bit(ctx, 13, 0);
		printf("%s:%i ret:%i\n", __FUNCTION__, __LINE__, ret);
		sleep(1);

	}

	return 0;
}

