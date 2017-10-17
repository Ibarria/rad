#pragma once

#include "SrcLocation.h"

class FileData
{
	char *data;
	char filename[256];
	size_t index;
	size_t size;
	size_t nline;
	size_t ncol;
public:
	FileData();
	~FileData();
	bool open(const char *filename);
	void close();
	bool getc(char &c);
	bool peek(char &c);
	void getLocation(SrcLocation &loc) const;
	const char *getFilename() const { return filename; }
};

