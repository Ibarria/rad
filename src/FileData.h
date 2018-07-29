#pragma once

#include "mytypes.h"
#include "SrcLocation.h"
#include "Array.h"

#if defined(getc)
#undef getc
#endif


class FileData
{
	char *data;
	char filename[256];
	u64 index;
	u64 size;
	u64 nline;
	u64 ncol;
    Array<char *> lines;
public:
	FileData();
	~FileData();
	bool open(const char *filename);
	bool loadString(const char *str, u64 num_chars);
	void close();
	bool getc(char &c);
	bool peek(char &c);
	void getLocation(SrcLocation &loc) const;
    void lookAheadTwo(char *in);
	const char *getFilename() const { return filename; }
    char * printLocation(const SrcLocation &loc, char *str) const;
};

