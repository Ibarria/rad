#pragma once
#include "mytypes.h"
#include "Array.h"
#include "TextType.h"

template <class Key, class Value>
class HashNode {
    Key       _key;
    Value     _value;
    HashNode *_next;

public:
    HashNode(const Key &key, const Value &value) :
        _key(key), _value(value), _next(NULL) {
    }

    Key key() const {
        return _key;
    }

    Value value() const {
        return _value;
    }

    void setValue(Value value) {
        _value = value;
    }

    HashNode *next() const {
        return _next;
    }

    void setNext(HashNode *next) {
        _next = next;
    }
};

u32 murmur3_32(const u8* key, u64 len, u32 seed);

template <class Key, u32 size>
struct HashFunc {
    u32 operator()(const Key &key) const
    {
        return murmur3_32(&key, sizeof(key), 0x3245AAFF);
    }
};

template <u32 size>
struct TextTypeHashFunc {
    u32 operator()(const TextType &key) const
    {
        const char *s;
        u32 len = 0;
        s = key;
        while (*s) { len++; s++; }
        return murmur3_32((const u8 *)key, len, 0x3245AAFF);
    }
};

template <class Key>
struct KeyComp {
    bool operator()(const Key &key1, const Key &key2) const
    {
        return key1 == key2;
    }
};

struct TextTypeComp {
    bool operator()(const TextType &text1, const TextType &text2)
    {
        const char *t1, *t2;
        t1 = text1;
        t2 = text2;
        while (*t1 && *t2) {
            if (*t1 != *t2) return false;
            t1++;
            t2++;
        }
        return *t1 == *t2;
    }
};

template <class Key, class Value, u32 size, 
    typename Func = HashFunc<Key, size>, typename Comp = KeyComp<Key> >
class Hash
{
    HashNode<Key, Value> *nodes[size];
    Func hFunc;
    Comp kComp;

public:
    struct HashIter {
        HashNode<Key, Value> *entry;
        u32 index;

        HashIter(HashNode<Key, Value> *_entry, u32 _index) {
            entry = _entry;
            index = _index;
        }
    };

    Hash(): nodes(), hFunc(), kComp() {

    }

    ~Hash() {
        for (u32 i = 0; i < size; ++i) {
            HashNode<Key, Value> *entry = nodes[i];

            while (entry != NULL) {
                HashNode<Key, Value> *prev = entry;
                entry = entry->next();
                delete prev;
            }

            nodes[i] = NULL;
        }
    }

    HashIter begin() {
        return HashIter(nodes[0], 0);
    }

    bool isEnd(HashIter iter) {
        return (iter.index >= size);
    }

    HashIter next(HashIter iter) {
        if (isEnd(iter)) return iter;
        if (iter.entry) {
            if (iter.entry->next()) return HashIter(iter.entry->next(), iter.index);
        }
        // If we get here, we need to look for a new index
        while (iter.index < size) {
            iter.index++;
            if (nodes[iter.index]) return HashIter(nodes[iter.index], iter.index);
        }
        return HashIter(nullptr, size);
    }

    bool get(const Key &key, Value &val)
    {
        u32 hashValue = hFunc(key) % size;
        HashNode<Key, Value> *entry = nodes[hashValue];

        while (entry != NULL) {
            if (kComp(entry->key(), key)) {
                val = entry->value();
                return true;
            }

            entry = entry->next();
        }

        return false;
    }

    void put(const Key &key, const Value &val)
    {
        u32 hashValue = hFunc(key) % size;
        HashNode<Key, Value> *prev = NULL;
        HashNode<Key, Value> *entry = nodes[hashValue];

        while (entry != NULL && !kComp(entry->key(), key)) {
            prev = entry;
            entry = entry->next();
        }

        if (entry == NULL) {
            entry = new HashNode<Key, Value>(key, val);

            if (prev == NULL) {
                // insert as first bucket
                nodes[hashValue] = entry;

            } else {
                prev->setNext(entry);
            }

        } else {
            // just update the value
            entry->setValue(val);
        }
    }

    void remove(const Key &key)
    {
        u32 hashValue = hFunc(key) % size;
        HashNode<Key, Value> *prev = NULL;
        HashNode<Key, Value> *entry = nodes[hashValue];

        while (entry != NULL && !kComp(entry->key(), key)) {
            prev = entry;
            entry = entry->next();
        }

        if (entry == NULL) {
            // key not found
            return;
        } else {
            if (prev == NULL) {
                // remove first bucket of the list
                nodes[hashValue] = entry->next();
            } else {
                prev->setNext(entry->next());
            }
            delete entry;
        }
    }
};

typedef Hash<TextType, bool, 21, TextTypeHashFunc<21>, TextTypeComp> ImportsHash;