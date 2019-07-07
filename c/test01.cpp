
#include <cstdio>
#include <cstdint>

// -----------------------------------------------------------------------------

const unsigned bitsImgLength = 32;
uint8_t bits[bitsImgLength];

const unsigned wordsCount = 16;
uint16_t words[wordsCount];

uint8_t test01[] = { 0x12, 0x34, 0x56 };

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

//long int strtol(const char *nptr, char **endptr, int base);

int main () {
    printf("%s:%i\n", __FUNCTION__, __LINE__);
    return 0;
}
