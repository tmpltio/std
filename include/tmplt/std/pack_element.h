#ifndef TMPLT_STD_PACK_ELEMENT_H
#define TMPLT_STD_PACK_ELEMENT_H

#include <cstddef>

namespace tmplt::std
{

template<::std::size_t I, typename ... Pack>
requires (I < sizeof...(Pack))
struct pack_element
{
private:
    template<::std::size_t J, typename, typename ... Rest>
    struct unpack : unpack<J - 1, Rest...>
    {};

    template<typename Current, typename ... Rest>
    struct unpack<0, Current, Rest...>
    {
        using type = Current;
    };

public:
    using type = typename unpack<I, Pack...>::type;
};

template<::std::size_t I, typename ... Pack>
using pack_element_t = typename pack_element<I, Pack...>::type;

}

#endif /* TMPLT_STD_PACK_ELEMENT_H */
