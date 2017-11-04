#pragma once

#include "mytypes.h"
#include "SrcLocation.h"

class FileData
{
	char *data;
	char filename[256];
	u64 index;
	u64 size;
	u64 nline;
	u64 ncol;
public:
	FileData();
	~FileData();
	bool open(const char *filename);
	void close();
	bool getc(char &c);
	bool peek(char &c);
	void getLocation(SrcLocation &loc) const;
    void lookAheadTwo(char *in);
	const char *getFilename() const { return filename; }
};

