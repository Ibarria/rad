#include "Hash.h"

u32 murmur3_32(const u8* key, u64 len, u32 seed) {
    u32 h = seed;
    if (len > 3) {
        const u32* key_x4 = (const u32*)key;
        size_t i = len >> 2;
        do {
            u32 k = *key_x4++;
            k *= 0xcc9e2d51;
            k = (k << 15) | (k >> 17);
            k *= 0x1b873593;
            h ^= k;
            h = (h << 13) | (h >> 19);
            h = (h * 5) + 0xe6546b64;
        } while (--i);
        key = (const u8*)key_x4;
    }
    if (len & 3) {
        u64 i = len & 3;
        u32 k = 0;
        key = &key[i - 1];
        do {
            k <<= 8;
            k |= *key--;
        } while (--i);
        k *= 0xcc9e2d51;
        k = (k << 15) | (k >> 17);
        k *= 0x1b873593;
        h ^= k;
    }
    h ^= len;
    h ^= h >> 16;
    h *= 0x85ebca6b;
    h ^= h >> 13;
    h *= 0xc2b2ae35;
    h ^= h >> 16;
    return h;
}

