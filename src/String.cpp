#include "String.h"
#include <string.h>
#include <assert.h>

String::String(const char* s, Allocator* a)
{
	if (a == nullptr) {
		allocator = getMallocAllocator();
	} else {
		allocator = a;
	}

	data_size = 0;
	data = nullptr;
	resize(strlen(s) + 1);
	memcpy(data, s, data_size - 1);
	data[data_size - 1] = 0;
}

String::String(const String& str)
{
	allocator = str.allocator;
	data_size = 0;
	data = nullptr;
	if (str.data_size > 0) {
		resize(str.data_size);
		memcpy(data, str.data, str.data_size);
	}
}

String& String::operator =(const char* s)
{
	u64 new_size = strlen(s);
	if (new_size >= data_size) {
		free_resources();
		resize(new_size + 1);
	} else {
		data_size = new_size + 1;
	}

	memcpy(data, s, new_size);
	data[new_size] = 0;

	return *this;
}

String& String::operator =(const String& str)
{
	u64 new_size = str.data_size;
	if (new_size >= data_size) {
		free_resources();
		resize(new_size);
	} else {
		data_size = new_size;
	}

	memcpy(data, str.data, new_size);

	return *this;
}

bool String::operator ==(const char* s)
{
	u64 ssize = strlen(s);
	if (ssize + 1 != data_size) return false;
	return !memcmp(data, s, ssize);
}

bool String::operator ==(const String& str)
{
	if (str.data_size != data_size) return false;
	return !memcmp(data, str.data, data_size);
}

bool String::operator !=(const char* s)
{
	return !(*this == s);
}

bool String::operator !=(const String& str)
{
	return !(*this == str);
}


bool String::resize(u64 new_size)
{
	char* new_data = (char *)allocator->alloc(new_size);
	if (!new_data) {
		assert(!"Out of memory issue");
		return false;
	}
	if (data_size != 0) {
		memcpy(new_data, data, data_size);
		allocator->free(data);
	}
	data = new_data;
	data_size = new_size;
	return true;
}

void String::free_resources()
{
	if (data != nullptr) {
		allocator->free(data);
		data = nullptr;
	}
	data_size = 0;
}


String::~String()
{
	free_resources();
}
