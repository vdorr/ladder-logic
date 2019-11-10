
#include <stdint.h>
#include <stdio.h>

#include <string>
#include <fstream>
#include <streambuf>
#include <vector>
#include <iterator>

using namespace std;

// -----------------------------------------------------------------------------

const unsigned bitsImgLength = 32;
uint8_t bits[bitsImgLength];

const unsigned wordsCount = 16;
uint16_t words[wordsCount];

uint8_t blob[] = {
0x26, 0x0, 0x82, 0x60, 0x28, 0xa6, 0x1, 0xb8, 0x70, 0x20
, 0
};

/*
serial buffer is either 16 or 64 bytes
*/

// -----------------------------------------------------------------------------

void get4(const uint8_t* const blob, unsigned& nibbleAddr, uint8_t& dst) {
    const uint8_t byte = blob[nibbleAddr >> 1];
    dst = nibbleAddr & 1 ? (byte & 0x0f) : (byte >> 4);
    nibbleAddr += 1;
}

void get8(const uint8_t* const blob, unsigned& nibbleAddr, uint8_t& dst) {
    const unsigned addr = nibbleAddr >> 1;
    if ( nibbleAddr & 1 ) {
        dst = blob[addr] << 4 | blob[addr + 1] >> 4;
    } else {
        dst = blob[addr];
    }
    nibbleAddr += 2;
}

void get12(const uint8_t* const blob, unsigned& nibbleAddr, uint16_t& dst) {
    uint8_t l, h;
    get8(blob, nibbleAddr, l);
    get4(blob, nibbleAddr, h);
    dst = (uint16_t)h << 4 | l;
}

void get16(const uint8_t* const blob, unsigned& nibbleAddr, uint16_t& dst) {
    uint8_t l, h;
    get8(blob, nibbleAddr, l);
    get8(blob, nibbleAddr, h);
    dst = (uint16_t)h << 8 | l;
}

// -----------------------------------------------------------------------------

class ctx {
public:
    unsigned        addr;
    uint16_t        bstack;
    unsigned        bstack_ptr;
    vector<int16_t> wstack;
    unsigned        wstack_ptr;
    vector<bool>    bmem;
    vector<int16_t> wmem;

    ctx();
};

ctx::ctx()
: addr(0)
, bstack(0)
, bstack_ptr(0)
, wstack_ptr(0)
, bmem(256, false)
, wmem(16, 0)
{}

// -----------------------------------------------------------------------------

void op_jump(ctx & p, const uint16_t f0) {
	printf("%s:%i %i\n", __FUNCTION__, __LINE__, f0);
}
void op_return(ctx & p) {
	printf("%s:%i\n", __FUNCTION__, __LINE__);
}
void op_ITrap(ctx & p) {
	printf("%s:%i\n", __FUNCTION__, __LINE__);
}
void op_ILdOn(ctx & p) {
//	printf("%s:%i\n", __FUNCTION__, __LINE__);
//    p.bstack_ptr++;
    p.bstack = (p.bstack << 1) | 1;
}
void op_IDup(ctx & p) {
//	printf("%s:%i\n", __FUNCTION__, __LINE__);
    p.bstack = (p.bstack << 1) | (p.bstack & 1);
//    p.bstack_ptr++;
}
void op_IPick(ctx & p, const uint8_t f0) {
	printf("%s:%i %i\n", __FUNCTION__, __LINE__, f0);
//    p.bstack_ptr++;
}
void op_IDrop(ctx & p) {
//	printf("%s:%i\n", __FUNCTION__, __LINE__);
//    p.bstack_ptr--;
    p.bstack = p.bstack >> 1;
}
void op_ILdBit(ctx & p, const uint8_t f0) {
//	printf("%s:%i %i\n", __FUNCTION__, __LINE__, f0);
    p.bstack = (p.bstack << 1) | p.bmem[f0];
}
void op_IStBit(ctx & p, const uint8_t f0) {
//	printf("%s:%i %i\n", __FUNCTION__, __LINE__, f0);
    p.bmem[f0] = p.bstack & 1;
    p.bstack = p.bstack >> 1;
}
void op_IAnd(ctx & p) {
//	printf("%s:%i\n", __FUNCTION__, __LINE__);
    p.bstack = ((p.bstack >> 1) & ~1) | ((p.bstack & (p.bstack >> 1)) & 1);
}
void op_IOr(ctx & p) {
	printf("%s:%i\n", __FUNCTION__, __LINE__);
}
void op_INot(ctx & p) {
//	printf("%s:%i\n", __FUNCTION__, __LINE__);
    p.bstack = p.bstack ^ 1;
}
void op_ILdCnA(ctx & p, const uint16_t f0) {
	printf("%s:%i %i\n", __FUNCTION__, __LINE__, f0);
}
void op_ILdM(ctx & p) {
	printf("%s:%i\n", __FUNCTION__, __LINE__);
}
void op_IStM(ctx & p) {
	printf("%s:%i\n", __FUNCTION__, __LINE__);
}
void op_IEq(ctx & p) {
	printf("%s:%i\n", __FUNCTION__, __LINE__);
}
void op_ILt(ctx & p) {
	printf("%s:%i\n", __FUNCTION__, __LINE__);
}
void op_IGt(ctx & p) {
	printf("%s:%i\n", __FUNCTION__, __LINE__);
}

int test01 () {
#if 0
unsigned addr = 0;
    uint8_t op;
    get4(blob, addr, op);
    while ( op ) {
        switch ( op ) {
#include "stub01.cpp"
        }
        get4(blob, addr, op);
    }
#endif
    return 0;
}

int loop ( ctx p, const vector<uint8_t> & str ) {

    const uint8_t* blob = str.data();

    unsigned addr = 0;
    uint8_t op;
    get4(blob, addr, op);
    while ( op ) {
        switch ( op ) {
#include "stub01.cpp"
        }
        get4(blob, addr, op);
    }
    return 0;
}

void test02 ( const string& fn ) {

    printf("%s:%i\n", __FUNCTION__, __LINE__);

    ifstream is(fn);

    istream_iterator<uint8_t> start(is), end;

    vector<uint8_t> str ( start, end );

    printf("%s:%i size:%i\n", __FUNCTION__, __LINE__, (int)str.size());

    ctx p;

    loop ( p, str );
}

int main ( int argc, char *argv[] ) {
	if ( 2 == argc ) {
		printf("%s:%i '%s'\n", __FUNCTION__, __LINE__, argv[1]);
		string fn(argv[1]);
		test02(fn);
	} else {
		printf("%s:%i\n", __FUNCTION__, __LINE__);
		test01();
	}
	return 0;
}
