#ifndef TMPLT_STD_TUPLE_VIEW_H
#define TMPLT_STD_TUPLE_VIEW_H

#include "meta_iota_view.h"
#include <tuple>
#include <cstddef>
#include <type_traits>
#include <ranges>
#include <concepts>
#include <utility>
#include <memory>
#include <iterator>
#include <functional>

namespace tmplt::std::ranges
{

namespace detail
{

template<typename Tuple, typename Element>
struct qualify_as_get
{
private:
    template<typename T>
    using const_t = ::std::conditional_t<::std::is_const_v<::std::remove_reference_t<Tuple>>, ::std::add_const_t<T>, T>;

    template<typename T>
    using reference_t = ::std::conditional_t<::std::is_lvalue_reference_v<Tuple>, ::std::add_lvalue_reference_t<T>, ::std::add_rvalue_reference_t<T>>;

public:
    using type = reference_t<const_t<Element>>;
};

template<typename Tuple, typename Element>
using qualify_as_get_t = typename qualify_as_get<Tuple, Element>::type;

template<typename T, ::std::size_t I>
concept member_getable = requires(T&& t)
{
    { ::std::forward<T>(t).template get<I>() } -> ::std::same_as<qualify_as_get_t<T, ::std::tuple_element_t<I, ::std::remove_cvref_t<T>>>>;
};

template<typename T, ::std::size_t I>
concept adl_getable = requires(T&& t)
{
    { get<I>(::std::forward<T>(t)) } -> ::std::same_as<qualify_as_get_t<T, ::std::tuple_element_t<I, ::std::remove_cvref_t<T>>>>;
};

template<typename T, ::std::size_t I>
concept getable = requires(T&& t)
{
    typename ::std::tuple_element<I, ::std::remove_cvref_t<T>>::type;
    requires member_getable<T, I> || adl_getable<T, I>;
};

template<typename T, ::std::size_t ... I>
requires (getable<T, I> && ...)
consteval void all_getable(::std::index_sequence<I...>);

}

template<typename T>
concept tuple_like = requires
{
    { ::std::tuple_size<::std::remove_cvref_t<T>>::value } -> ::std::convertible_to<::std::size_t>;
    detail::all_getable<T>(::std::make_index_sequence<::std::tuple_size_v<::std::remove_cvref_t<T>>>{});
};

template<typename Tuple>
requires tuple_like<::std::add_lvalue_reference_t<Tuple>>
class tuple_view : public ::std::ranges::view_interface<tuple_view<Tuple>>
{
    static void construction_check(Tuple&);

    static void construction_check(Tuple&&) = delete;

    static constexpr auto index_range = views::meta_iota<::std::tuple_size_v<::std::remove_cvref_t<Tuple>>>();

    class iterator
    {
        using index_iterator_t = ::std::ranges::iterator_t<decltype(index_range)>;
        using index_sentinel_t = ::std::ranges::sentinel_t<decltype(index_range)>;

        class tuple_visitor
        {
            template<typename Visitor>
            class caller
            {
                template<::std::size_t I>
                requires detail::member_getable<::std::add_lvalue_reference_t<Tuple>, I>
                [[nodiscard]] constexpr decltype(auto) get_element() const noexcept(noexcept(tuple->template get<I>()))
                {
                    return tuple->template get<I>();
                }

                template<::std::size_t I>
                requires detail::adl_getable<::std::add_lvalue_reference_t<Tuple>, I>
                [[nodiscard]] constexpr decltype(auto) get_element() const noexcept(noexcept(get<I>(*tuple)))
                {
                    return get<I>(*tuple);
                }

            public:
                template<::std::convertible_to<::std::add_rvalue_reference_t<Visitor>> T>
                explicit constexpr caller(Tuple* tuple, T&& visitor) noexcept : tuple{tuple}, visitor{::std::forward<T>(visitor)}
                {}

                template<typename Index>
                requires ::std::invocable<Visitor, detail::qualify_as_get_t<::std::add_lvalue_reference_t<Tuple>, ::std::tuple_element_t<Index::value, ::std::remove_cvref_t<Tuple>>>>
                constexpr decltype(auto) operator()(Index const&) const noexcept(noexcept(::std::invoke(::std::forward<Visitor>(visitor), get_element<Index::value>())))
                {
                    return ::std::invoke(::std::forward<Visitor>(visitor), get_element<Index::value>());
                }

            private:
                Tuple* tuple;
                ::std::add_rvalue_reference_t<Visitor> visitor;
            };

            template<typename Visitor>
            caller(Tuple*, Visitor&&) -> caller<Visitor>;

        public:
            explicit constexpr tuple_visitor(Tuple* tuple, index_iterator_t index) noexcept(noexcept(index_iterator_t{index})) : tuple{tuple}, index{index}
            {}

            template<typename Visitor>
            requires ::std::invocable<::std::iter_reference_t<::std::add_const_t<index_iterator_t>>, caller<Visitor>>
            constexpr decltype(auto) operator()(Visitor&& visitor) const noexcept(noexcept(::std::invoke(*index, caller{tuple, ::std::forward<Visitor>(visitor)})))
            {
                return ::std::invoke(*index, caller{tuple, ::std::forward<Visitor>(visitor)});
            }

        private:
            Tuple* tuple;
            index_iterator_t index;
        };

    public:
        using iterator_concept = ::std::random_access_iterator_tag;
        using value_type = tuple_visitor;
        using difference_type = ::std::ptrdiff_t;

        explicit constexpr iterator() = default;

        explicit constexpr iterator(Tuple* tuple, index_iterator_t index) noexcept(noexcept(index_iterator_t{index})) : tuple{tuple}, index{index}
        {}

        [[nodiscard]] constexpr value_type operator*() const noexcept(noexcept(value_type{tuple, index}))
        {
            return value_type{tuple, index};
        }

        [[nodiscard]] constexpr value_type operator[](difference_type diff) const noexcept(noexcept(*(*this + diff)))
        {
            return *(*this + diff);
        }

        constexpr iterator& operator++() noexcept(noexcept(++index))
        {
            ++index;

            return *this;
        }

        constexpr iterator operator++(int) noexcept(noexcept(iterator{*this}) && noexcept(++*this))
        {
            auto tmp = *this;
            ++*this;

            return tmp;
        }

        constexpr iterator& operator--() noexcept(noexcept(--index))
        {
            --index;

            return *this;
        }

        constexpr iterator operator--(int) noexcept(noexcept(iterator{*this}) && noexcept(--*this))
        {
            auto tmp = *this;
            --*this;

            return tmp;
        }

        constexpr iterator& operator+=(difference_type diff) noexcept(noexcept(index += ::std::size_t(diff)))
        {
            index += ::std::size_t(diff);

            return *this;
        }

        constexpr iterator& operator-=(difference_type diff) noexcept(noexcept(index -= ::std::size_t(diff)))
        {
            index -= ::std::size_t(diff);

            return *this;
        }

        [[nodiscard]] friend constexpr iterator operator+(iterator it, difference_type diff) noexcept(noexcept(it += diff))
        {
            return it += diff;
        }

        [[nodiscard]] friend constexpr iterator operator+(difference_type diff, iterator it) noexcept(noexcept(it + diff))
        {
            return it + diff;
        }

        [[nodiscard]] friend constexpr iterator operator-(iterator it, difference_type diff) noexcept(noexcept(it -= diff))
        {
            return it -= diff;
        }

        [[nodiscard]] friend constexpr difference_type operator-(iterator const& lhs, iterator const& rhs) noexcept(noexcept(difference_type(lhs.index - rhs.index)))
        {
            return difference_type(lhs.index - rhs.index);
        }

        [[nodiscard]] friend constexpr difference_type operator-(iterator const& it, index_sentinel_t const& sentinel) noexcept(noexcept(difference_type(it.index - sentinel)))
        {
            return difference_type(it.index - sentinel);
        }

        [[nodiscard]] friend constexpr difference_type operator-(index_sentinel_t const& sentinel, iterator const& it) noexcept(noexcept(difference_type(sentinel - it.index)))
        {
            return difference_type(sentinel - it.index);
        }

        [[nodiscard]] friend constexpr bool operator==(iterator const& lhs, iterator const& rhs) = default;

        [[nodiscard]] friend constexpr auto operator<=>(iterator const& lhs, iterator const& rhs) = default;

        [[nodiscard]] friend constexpr bool operator==(iterator const& it, index_sentinel_t const& sentinel) noexcept(noexcept(it.index == sentinel))
        {
            return it.index == sentinel;
        }

    private:
        Tuple* tuple{nullptr};
        index_iterator_t index{};
    };

public:
    template<tuple_like T>
    requires ::std::convertible_to<T, Tuple&> && requires { construction_check(::std::declval<T>()); }
    explicit constexpr tuple_view(T&& tuple) noexcept(noexcept(static_cast<Tuple&>(::std::declval<T>()))) : tuple{::std::addressof(static_cast<Tuple&>(::std::forward<T>(tuple)))}
    {}

    constexpr auto begin() const noexcept(noexcept(iterator{tuple, ::std::ranges::begin(index_range)}))
    {
        return iterator{tuple, ::std::ranges::begin(index_range)};
    }

    constexpr auto end() const noexcept(noexcept(::std::ranges::end(index_range)))
    {
        return ::std::ranges::end(index_range);
    }

private:
    Tuple* tuple;
};

template<typename Tuple>
tuple_view(Tuple&&) -> tuple_view<::std::remove_reference_t<Tuple>>;

namespace views
{

struct tuple_view_factory
{
    template<tuple_like Tuple>
    [[nodiscard]] constexpr auto operator()(Tuple&& tuple) const noexcept(noexcept(tuple_view{::std::forward<Tuple>(tuple)}))
    {
        return tuple_view{::std::forward<Tuple>(tuple)};
    }
};

inline constexpr auto tuple = tuple_view_factory{};

}

}

template<typename Tuple>
inline constexpr bool ::std::ranges::enable_borrowed_range<::tmplt::std::ranges::tuple_view<Tuple>> = true;

#endif /* TMPLT_STD_TUPLE_VIEW_H */
