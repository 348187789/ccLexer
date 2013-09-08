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



bool regex_match(const std::string& toMatch, std::string re);


class lexIterator
{
public:
    lexIterator()
    {
        data.second = nonterminalFlagOfDfaState;
    }

    const std::pair<std::string, int>& operator*();

    lexIterator& operator++(); // 只支持前置形式

    bool operator==(const lexIterator& other);

    bool operator!=(const lexIterator& other);

    operator bool()
    {
        return data.second != nonterminalFlagOfDfaState;
    }

    lexIterator(std::shared_ptr<std::vector<dfaState> > inputDfa,
        std::istream& inputStream)
        : pFromStream(&inputStream)
        , dfa(inputDfa)
    {
        operator++();
    }

private:
    std::istream* pFromStream;
    std::shared_ptr<std::vector<dfaState> > dfa;
    std::pair<std::string, int> data;
};


class lexer
{
public:
	lexer() : lastId(-1), compiled(false){}

    int regist(const std::string& re)
    {
        assure(!compiled, "lexer has already been compiled!");
        regexes.push_back(re);
		return ++lastId;
    }

    int regist(std::string&& re)
    {
        assure(!compiled, "lexer has already been compiled!");
        regexes.push_back(std::move(re));
		return ++lastId;
    }

    void compile();

    lexIterator getIterator(std::basic_istream<char>& inputIstream)
    {
        if(!compiled)
        {
            compile();
        }
        return lexIterator(dfa, inputIstream);
    }

private:
    void assure(bool expr, const char* message)
    {
        if(!expr)
        {
            throw std::exception(message);
        }
    }
	int lastId;
    bool compiled;
    std::vector<std::string> regexes;
    std::shared_ptr<std::vector<dfaState> > dfa;
};


// std::vector<dfaState> 就是最终的dfa，vec[0] 是初始节点

} // namespace ccDetail

namespace cc
{
	using ccDetail::regex_match;
	using ccDetail::lexIterator;
	using ccDetail::lexer;
}



#endif