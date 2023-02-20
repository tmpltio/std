#ifndef TMPLT_STD_META_IOTA_VIEW_H
#define TMPLT_STD_META_IOTA_VIEW_H

#include <cstddef>
#include <ranges>
#include <concepts>
#include <variant>
#include <array>
#include <utility>
#include <type_traits>
#include <iterator>

namespace tmplt::std::ranges
{

template<::std::size_t Count>
class meta_iota_view : public ::std::ranges::view_interface<meta_iota_view<Count>>
{
    template<::std::size_t ... I>
    class callable_visit
    {
        using variant_t = ::std::variant<::std::integral_constant<::std::size_t, I>...>;

    public:
        template<typename Index>
        requires (::std::same_as<::std::remove_cvref_t<Index>, ::std::in_place_index_t<I>> || ...)
        explicit constexpr callable_visit(Index&& index) noexcept : variant{::std::forward<Index>(index)}
        {}

        template<typename Visitor>
        requires requires(Visitor&& visitor, variant_t variant)
        {
            ::std::visit(::std::forward<Visitor>(visitor), variant);
        }
        constexpr decltype(auto) operator()(Visitor&& visitor) const noexcept(noexcept(::std::visit(::std::forward<Visitor>(visitor), variant)))
        {
            return ::std::visit(::std::forward<Visitor>(visitor), variant);
        }

    private:
        variant_t variant;
    };

    template<::std::size_t ... I>
    [[nodiscard]] static consteval auto make_values(::std::index_sequence<I...>) noexcept
    {
        return ::std::array{callable_visit<I...>{::std::in_place_index<I>}...};
    }

    static constexpr auto values = make_values(::std::make_index_sequence<Count>{});

    class iterator
    {
    public:
        using iterator_concept = ::std::random_access_iterator_tag;
        using value_type = ::std::ranges::range_value_t<decltype(values)>;
        using difference_type = ::std::ptrdiff_t;

        explicit constexpr iterator() = default;

        [[nodiscard]] constexpr value_type operator*() const noexcept(noexcept(value_type{values[index]}))
        {
            return values[index];
        }

        [[nodiscard]] constexpr value_type operator[](difference_type diff) const noexcept(noexcept(*(*this + diff)))
        {
            return *(*this + diff);
        }

        constexpr iterator& operator++() noexcept
        {
            ++index;

            return *this;
        }

        constexpr iterator operator++(int) noexcept
        {
            auto tmp = *this;
            ++*this;

            return tmp;
        }

        constexpr iterator& operator--() noexcept
        {
            --index;

            return *this;
        }

        constexpr iterator operator--(int) noexcept
        {
            auto tmp = *this;
            --*this;

            return tmp;
        }

        constexpr iterator& operator+=(difference_type diff) noexcept
        {
            index += ::std::size_t(diff);

            return *this;
        }

        constexpr iterator& operator-=(difference_type diff) noexcept
        {
            index -= ::std::size_t(diff);

            return *this;
        }

        [[nodiscard]] friend constexpr iterator operator+(iterator it, difference_type diff) noexcept
        {
            return it += diff;
        }

        [[nodiscard]] friend constexpr iterator operator+(difference_type diff, iterator it) noexcept
        {
            return it + diff;
        }

        [[nodiscard]] friend constexpr iterator operator-(iterator it, difference_type diff) noexcept
        {
            return it -= diff;
        }

        [[nodiscard]] friend constexpr difference_type operator-(iterator const& lhs, iterator const& rhs) noexcept
        {
            return difference_type(lhs.index - rhs.index);
        }

        [[nodiscard]] friend constexpr difference_type operator-(iterator const& it, ::std::default_sentinel_t const&) noexcept
        {
            return difference_type(it.index - Count);
        }

        [[nodiscard]] friend constexpr difference_type operator-(::std::default_sentinel_t const&, iterator const& it) noexcept
        {
            return difference_type(Count - it.index);
        }

        [[nodiscard]] friend constexpr bool operator==(iterator const& lhs, iterator const& rhs) = default;

        [[nodiscard]] friend constexpr auto operator<=>(iterator const& lhs, iterator const& rhs) = default;

        [[nodiscard]] friend constexpr bool operator==(iterator const& it, ::std::default_sentinel_t const&) noexcept
        {
            return it.index == Count;
        }

    private:
        ::std::size_t index{};
    };

public:
    static constexpr auto begin() noexcept(noexcept(iterator{}))
    {
        return iterator{};
    }

    static constexpr auto end() noexcept(noexcept(::std::default_sentinel_t{::std::default_sentinel}))
    {
        return ::std::default_sentinel;
    }
};

namespace views
{

template<::std::size_t Count>
struct meta_iota_view_factory
{
    [[nodiscard]] constexpr auto operator()() const noexcept(noexcept(meta_iota_view<Count>{}))
    {
        return meta_iota_view<Count>{};
    }
};

template<::std::size_t Count>
inline constexpr auto meta_iota = meta_iota_view_factory<Count>{};

}

}

template<::std::size_t Count>
inline constexpr bool ::std::ranges::enable_borrowed_range<::tmplt::std::ranges::meta_iota_view<Count>> = true;

#endif /* TMPLT_STD_META_IOTA_VIEW_H */
