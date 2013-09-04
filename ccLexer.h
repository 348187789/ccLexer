#ifndef CCLEXER_H_INCLUDED
#define CCLEXER_H_INCLUDED

#include <string>
#include <vector>


namespace ccDetail
{


const int nonterminalFlagOfDfaState   = std::numeric_limits<int>::max();
const int nextDfaIsNullFlag = -1;

struct dfaState
{
    int terminalId;
    std::vector<int> next;

    dfaState()
        : terminalId(nonterminalFlagOfDfaState)
        , next(127, nextDfaIsNullFlag)
    {
    }

    dfaState(dfaState&& other) 
    {
        terminalId = other.terminalId;
        next = std::move(other.next);
    }

    void operator=(dfaState&& other)
    {
        terminalId = other.terminalId;
        next = std::move(other.next);
    }
};

// std::vector<dfaState> 就是最终的dfa，vec[0] 是初始节点


class lexer;

class lexerIterator
{
public:

    typedef std::string::const_iterator strIterType;
    typedef std::pair<strIterType, strIterType> strRangeType;

private:
    friend class lexer;
    lexerIterator(const std::string& inputStr, const std::vector<dfaState>& inputDfaVecReference)
        : dfaVecReference(inputDfaVecReference)
        , rangeOfMatchedContent(inputStr.begin(), inputStr.begin())
        , terminalId(-1)
        , toMatch(inputStr)
    {
    }

public:
    // 复制构造和赋值构造用默认的就可以
    const lexerIterator& operator++();
    std::pair<std::string, int> operator*();

    strRangeType getRange()
    {
        return rangeOfMatchedContent;
    }

	int getTagId()
	{
		return terminalId;
	}

    operator bool() const
    {
        return rangeOfMatchedContent.first != rangeOfMatchedContent.second;
    }

private:
    const std::vector<dfaState>& dfaVecReference;
    strRangeType rangeOfMatchedContent;
    int terminalId;
    const std::string& toMatch;
};

class lexer
{
public:
	lexer() : registered(false), nextId(0) {}

    typedef lexerIterator iterator;

	int regist(std::string&& inputRegexStr);

	int regist(const std::string& inputRegexStr); 

	void endRegist();

    iterator getIterator(const std::string* strPtrToMatch) const; 
	// todo : 增加从文本文件中获得iterator的重载版本

private:

    void dfaInit();

	bool registered;
	int nextId;
    std::vector<std::string> strVec;
    std::vector<dfaState>    dfaVec;

	lexer(const lexer&);
	void operator=(const lexer&);
};

} // namespace ccDetail


namespace cc
{
	using ccDetail::lexer;
	using ccDetail::lexerIterator;
}


#endif