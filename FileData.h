#pragma once

class FileData
{
	char *data;
	size_t index;
	size_t size;
public:
	FileData();
	~FileData();
	bool open(const char *filename);
	void close();
	bool getc(char &c);
	bool peek(char &c);
	void rewind_one();

};

