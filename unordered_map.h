#pragma once

#include <memory>
#include <vector>
#include <cmath>


template<typename T, typename Alloc = std::allocator<T>>
class List {
private:
    struct BaseNode {
        BaseNode* prev;
        BaseNode* next;

        BaseNode(): prev(this), next(this) {}

        void swap(BaseNode& other) {
            std::swap(prev, other.prev);
            std::swap(next, other.next);
        }
    };

    struct Node: BaseNode {
        T value;

        Node() = default;

        Node(const T& value) : BaseNode(), value(value) {}

        Node(T&& value) : value(std::move(value)) {}

        void swap(Node& other) {
            std::swap(this->prev, other.prev);
            std::swap(this->next, other.next);
            std::swap(value, other.value);
        }
    };

    using NodeAlloc = typename std::allocator_traits<Alloc>::template rebind_alloc<Node>;
    using AllocTraits = typename std::allocator_traits<NodeAlloc>;
    BaseNode m_end;
    BaseNode* m_begin;
    size_t m_size;
    [[no_unique_address]] NodeAlloc m_alloc;

    void copyElements(const List& other) { //only to empty list
        try {
            for (auto el = other.begin(); el != other.end(); ++el)
                push_back(*el);
        } catch(...) {
            clear();
            throw;
        }
    }

    void swap_without_alloc(List& other) {
        auto m_end_prev = m_end.prev;
        auto m_end_next = m_end.next;
        auto other_m_end_prev = other.m_end.prev;
        auto other_m_end_next = other.m_end.next;
        std::swap(m_end_prev->next, other_m_end_prev->next);
        std::swap(m_end_next->prev, other_m_end_next->prev);
        std::swap(m_end, other.m_end);
        m_begin = m_end.next;
        other.m_begin = other.m_end.next;
        std::swap(m_size, other.m_size);
    }

public:
    List(Alloc alloc) : m_end(), m_begin(&m_end), m_size(0), m_alloc(alloc) {}

    //In this and all the next nolint's clang-tidy thinks, that not all the fields are initialized
    //NOLINTNEXTLINE
    List() : List(Alloc()) {}

    void push_back(const T& value) {
        emplace_back(value);
    }

    void push_back(T&& value) {
        emplace_back(std::move(value));
    }

    template<class... Args>
    void emplace_back(Args&&... args) {
        emplace(end(), std::forward<Args>(args)...);
    }

    void pop_back() noexcept {
        erase(--end());
    }

    void clear() noexcept {
        while (!empty())
            pop_back();
    }

    //NOLINTNEXTLINE
    List(size_t count, const T& value, Alloc alloc): List(alloc) {
        try {
            for (size_t i = 0; i < count; ++i)
                push_back(value);
        }
        catch(...) {
            clear();
            throw;
        }
    }

    //NOLINTNEXTLINE
    List(size_t count, const T& value): List(count, value, Alloc()) {}

    //NOLINTNEXTLINE
    List(size_t count, Alloc alloc): List(count, T(), alloc) {}

    //NOLINTNEXTLINE
    List(size_t count): List(count, T()) {}

    NodeAlloc get_allocator() const noexcept {
        return m_alloc;
    }

    //NOLINTNEXTLINE/home/vorakt/CLionProjects/Deque
    List(const List& other): List(AllocTraits::select_on_container_copy_construction(other.m_alloc)) {
        copyElements(other);
    }

    ~List() {
        clear();
    }

    List(List&& other) : m_end(), m_begin(&m_end), m_size(0),
                         m_alloc(AllocTraits::select_on_container_copy_construction(other.m_alloc)) {
        swap_without_alloc(other);
    }

    List& operator=(const List& other) {
        auto new_alloc = AllocTraits::propagate_on_container_copy_assignment::value ? other.m_alloc : m_alloc;
        List copy(new_alloc);
        copy.copyElements(other);
        swap_without_alloc(copy);
        if constexpr (AllocTraits::propagate_on_container_copy_assignment::value)
            m_alloc = other.m_alloc;
        return *this;
    }

    List& operator=(List&& other) noexcept {
        if constexpr (AllocTraits::propagate_on_container_move_assignment::value) {
            List copy(std::move(other.m_alloc));
            copy.copyElements(other);
            swap_without_alloc(copy);
            other.clear();
        }
        else {
            List copy = std::move(other);
            swap_without_alloc(copy);
        }
        return *this;
    }

    size_t size() const noexcept {
        return m_size;
    }

    bool empty() const noexcept {
        return m_size == 0;
    }

    template<class... Args>
    void emplace_front(Args&&... args) {
        emplace(begin(), std::forward<Args>(args)...);
    }

    void push_front(const T& value) {
        emplace_front(value);
    }

    void push_front(T&& value) {
        emplace_front(std::move(value));
    }

    void pop_front() noexcept {
        erase(end());
    }

    template<bool IsConst>
    struct BaseIterator {
        template<class... Args>
        friend BaseIterator<false> List::emplace(BaseIterator<true> it, Args&&... args);

        friend void List::splice(BaseIterator<true>, List<T, Alloc>&, BaseIterator<true>) noexcept;

        friend void List::erase(BaseIterator<true>) noexcept;

    private:
        using node_type = std::conditional_t<IsConst, const Node, Node>;
        using const_node_type = const Node;
        BaseNode* m_node;

    public:
        using value_type = std::conditional_t<IsConst, const T, T>;
        using reference = value_type&;
        using iterator_category = std::bidirectional_iterator_tag;
        using pointer = value_type*;
        using difference_type = std::ptrdiff_t;

        BaseIterator(BaseNode* node): m_node(node) {}

        BaseIterator(): m_node(nullptr) {}

        operator BaseIterator<true>() const {
            return BaseIterator<true>(m_node);
        }

        value_type& operator*() const {
            return static_cast<node_type*>(m_node)->value;
        }

        value_type* operator->() const {
            return &(**this);
        }

        BaseIterator& operator++() {
            m_node = m_node->next;
            return *this;
        }

        BaseIterator operator++(int) {
            auto copy = *this;
            ++(*this);
            return copy;
        }

        BaseIterator& operator--() {
            m_node = m_node->prev;
            return *this;
        }

        BaseIterator operator--(int) {
            auto copy = *this;
            --(*this);
            return copy;
        }

        bool operator==(BaseIterator other) {
            return m_node == other.m_node;
        }
    };

    using iterator = BaseIterator<false>;
    using const_iterator = BaseIterator<true>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    iterator begin() noexcept {
        return iterator(m_begin);
    }

    const_iterator begin() const noexcept {
        return const_iterator(m_begin);
    }

    const_iterator cbegin() const noexcept {
        return const_iterator(m_begin);
    }

    iterator end() noexcept {
        return iterator(m_begin->prev);
    }

    const_iterator end() const noexcept {
        return const_iterator(m_begin->prev);
    }

    const_iterator cend() const noexcept {
        return const_iterator(m_begin->prev);
    }

    auto rbegin() noexcept {
        return std::reverse_iterator<iterator>(end());
    }

    auto rbegin() const noexcept {
        return std::reverse_iterator<const_iterator>(end());
    }

    auto rend() noexcept {
        return std::reverse_iterator<iterator>(begin());
    }

    auto rend() const noexcept {
        return std::reverse_iterator<const_iterator>(begin());
    }

    auto crbegin() const noexcept {
        return std::reverse_iterator<const_iterator>(end());
    }

    auto crend() const noexcept {
        return std::reverse_iterator<const_iterator>(begin());
    }

    iterator insert(const_iterator it, const T& value) {
        return emplace(it, value);
    }

    iterator insert(const_iterator it, T&& value) {
        return emplace(it, std::move(value));
    }

    template<class... Args>
    iterator emplace(const_iterator it, Args&&... args) {
        auto node = AllocTraits::allocate(m_alloc, 1);
        try {
            AllocTraits::construct(m_alloc, node, std::forward<Args>(args)...);
        } catch (...) {
            AllocTraits::deallocate(m_alloc, node, 1);
            throw;
        }
        it.m_node->prev->next = node;
        node->prev = it.m_node->prev;
        it.m_node->prev = node;
        node->next = it.m_node;
        ++m_size;
        m_begin = m_end.next;
        return iterator(node);
    }

    void splice(const_iterator pos, List& other, const_iterator it) noexcept {
        auto prev = it.m_node->prev;
        auto next = it.m_node->next;
        prev->next = next;
        next->prev = prev;
        --other.m_size;
        other.m_begin = other.m_end.next;
        it.m_node->next = pos.m_node;
        it.m_node->prev = pos.m_node->prev;
        pos.m_node->prev->next = it.m_node;
        pos.m_node->prev = it.m_node;
        ++m_size;
        m_begin = m_end.next;
    }

    void erase(const_iterator it) noexcept {
        auto prev = it.m_node->prev;
        auto next = it.m_node->next;
        prev->next = next;
        next->prev = prev;
        --m_size;
        m_begin = m_end.next;
        auto node = static_cast<Node*>(it.m_node);
        AllocTraits::destroy(m_alloc, node);
        AllocTraits::deallocate(m_alloc, node, 1);
    }

    void swap(List& other) {
        swap_without_alloc(other);
        if constexpr (AllocTraits::propagate_on_container_swap::value)
            std::swap(m_alloc, other.m_alloc);
    }
};



template<typename Key,
        typename Value,
        typename Hash = std::hash<Key>,
        typename Equal = std::equal_to<Key>,
        typename Alloc = std::allocator< std::pair<const Key, Value> >
>
class UnorderedMap {
public:
    using NodeType = std::pair<const Key, Value>;

    template<bool IsConst>
    struct BaseIterator;

private:
    const static size_t starting_table_size = 8;

    struct ListValue {
        std::pair<Key, Value> data;
        size_t hash;

        ListValue(const std::pair<Key, Value>& node, size_t hash) : data(node), hash(hash) {}

        ListValue(std::pair<Key, Value>&& node, size_t hash) : data(std::move(node)), hash(hash) {}
    };

    using MapList = List<ListValue, Alloc>;
    MapList list;
    std::vector<typename MapList::iterator,
            typename std::allocator_traits<Alloc>::template rebind_alloc<typename MapList::iterator>> table;
    float m_max_load_factor;
    [[no_unique_address]] Hash hash;
    [[no_unique_address]] Equal equal;

    template<typename U>
    std::pair<BaseIterator<false>, bool> universal_insert(U&& node) {
        if (static_cast<float>(size()) + 1 > m_max_load_factor * static_cast<float>(table.size()))
            reserve(2 * size() + 1);
        auto find_result = find_in_bucket(node.first);
        if (find_result.first)
            return { find_result.second, false };
        size_t node_hash = hash(node.first);
        auto iterator_result = list.insert(find_result.second, ListValue(std::forward<U>(node), node_hash));
        if (table[node_hash % table.size()] == typename MapList::iterator())
            table[node_hash % table.size()] = iterator_result;
        return { iterator_result, true };
    }

public:
    UnorderedMap() : list(), table(), m_max_load_factor(1), hash(), equal() {
        table.resize(starting_table_size, typename MapList::iterator());
    }

    UnorderedMap(const UnorderedMap& other) : list(other.list), table(), m_max_load_factor(other.m_max_load_factor),
                                              hash(other.hash), equal(other.equal) {
        table.resize(other.table.size(), typename MapList::iterator());
        for (auto it = list.begin(); it != list.end(); ++it)
            if (table[it->hash % table.size()] == typename MapList::iterator())
                table[it->hash % table.size()] = it;
    }

    UnorderedMap(UnorderedMap&& other) : list(std::move(other.list)), table(std::move(other.table)),
                                         m_max_load_factor(other.m_max_load_factor), hash(std::move(other.hash)),
                                         equal(std::move(other.equal)) {
        other.m_max_load_factor = 1;
    }

    ~UnorderedMap() = default;

    UnorderedMap& operator=(const UnorderedMap& other) {
        list = other.list;
        table.assign(other.table.size(), typename MapList::iterator());
        for (auto it = list.begin(); it != list.end(); ++it)
            if (table[it->hash % table.size()] == typename MapList::iterator())
                table[it->hash % table.size()] = it;
        m_max_load_factor = other.m_max_load_factor;
        hash = other.hash;
        equal = other.equal;
        return *this;
    }

    UnorderedMap& operator=(UnorderedMap&& other) {
        list = std::move(other.list);
        table = std::move(other.table);
        m_max_load_factor = other.m_max_load_factor;
        other.m_max_load_factor = 1;
        return *this;
    }

    size_t size() const noexcept {
        return list.size();
    }

    auto get_allocator() const noexcept {
        return list.get_allocator();
    }

    template<bool IsConst>
    struct BaseIterator {
    private:
        using node_type = std::conditional_t<IsConst, typename MapList::const_iterator, typename MapList::iterator>;
        node_type m_iterator;

    public:
        using value_type = std::conditional_t<IsConst, const NodeType, NodeType>;
        using reference = value_type&;
        using iterator_category = std::bidirectional_iterator_tag;
        using pointer = value_type*;
        using difference_type = std::ptrdiff_t;

        BaseIterator(): m_iterator() {}

        BaseIterator(typename MapList::iterator it): m_iterator(it) {}

        BaseIterator(typename MapList::const_iterator it): m_iterator(it) {}

        operator BaseIterator<true>() const {
            return BaseIterator<true>(m_iterator);
        }

        value_type& operator*() const {
            return *reinterpret_cast<value_type*>(&(m_iterator->data));
        }

        value_type* operator->() const {
            return &(**this);
        }

        BaseIterator& operator++() {
            ++m_iterator;
            return *this;
        }

        BaseIterator operator++(int) {
            auto copy = *this;
            ++(*this);
            return copy;
        }

        BaseIterator& operator--() {
            --m_iterator;
            return *this;
        }

        BaseIterator operator--(int) {
            auto copy = *this;
            --(*this);
            return copy;
        }

        bool operator==(BaseIterator other) const {
            return m_iterator == other.m_iterator;
        }

        size_t hash() const noexcept {
            return m_iterator->hash;
        }

        node_type getListIterator() noexcept {
            return m_iterator;
        }

        node_type getListIterator() const noexcept {
            return m_iterator;
        }
    };


    using iterator = BaseIterator<false>;
    using const_iterator = BaseIterator<true>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    iterator begin() noexcept {
        return iterator(list.begin());
    }

    const_iterator begin() const noexcept {
        return const_iterator(list.begin());
    }

    const_iterator cbegin() const noexcept {
        return const_iterator(list.begin());
    }

    iterator end() noexcept {
        return iterator(list.end());
    }

    const_iterator end() const noexcept {
        return const_iterator(list.end());
    }

    const_iterator cend() const noexcept {
        return const_iterator(list.end());
    }

    auto rbegin() noexcept {
        return std::reverse_iterator<iterator>(end());
    }

    auto rbegin() const noexcept {
        return std::reverse_iterator<const_iterator>(end());
    }

    auto rend() noexcept {
        return std::reverse_iterator<iterator>(begin());
    }

    auto rend() const noexcept {
        return std::reverse_iterator<const_iterator>(begin());
    }

    auto crbegin() const noexcept {
        return std::reverse_iterator<const_iterator>(end());
    }

    auto crend() const noexcept {
        return std::reverse_iterator<const_iterator>(begin());
    }

private:
    std::pair<bool, typename MapList::iterator> find_in_bucket(const Key& value) noexcept {
        size_t value_hash = hash(value);
        auto it = table[value_hash % table.size()];
        if (it == typename MapList::iterator())
            return { false, list.end() };
        for (; it != list.end() && it->hash % table.size() == value_hash % table.size(); ++it) {
            if (equal(it->data.first, value))
                return { true, it };
        }
        return { false, it };
    }

    std::pair<bool, const_iterator> find_in_bucket(const Key& value) const noexcept {
        return find_in_bucket(value);
    }

public:
    void reserve(size_t count) noexcept {
        if (static_cast<float>(count) <= m_max_load_factor * static_cast<float>(table.size()))
            return;
        count = static_cast<size_t>(std::ceil(static_cast<float>(count) / m_max_load_factor));
        table.clear();
        table.resize(count, typename MapList::iterator());
        MapList old_list = std::move(list);
        while (!old_list.empty()) {
            auto it = old_list.begin();
            size_t pos = it->hash % count;
            if (table[pos] == typename MapList::iterator()) {
                list.splice(list.begin(), old_list, it);
                table[pos] = list.begin();
            }
            else {
                list.splice(table[pos], old_list, it);
                --table[pos];

            }
        }
    }

    std::pair<iterator, bool> insert(const std::pair<Key, Value>& node) {
        return universal_insert(node);
    }

    std::pair<iterator, bool> insert(std::pair<Key, Value>&& node) {
        return universal_insert(std::move(node));
    }

    template<class InputIt>
    void insert(InputIt first, InputIt last) {
        for (auto it = first; it != last; ++it)
            insert(*it);
    }

    template<typename... Args>
    std::pair<iterator, bool> emplace(Args&&... args) {
        using PairAlloc = typename std::allocator_traits<Alloc>::template rebind_alloc<std::pair<Key, Value>>;
        using AllocTraits = typename std::allocator_traits<PairAlloc>;
        PairAlloc alloc = list.get_allocator();
        auto node = AllocTraits::allocate(alloc, 1);
        std::pair<UnorderedMap::BaseIterator<false>, bool> insert_result;
        try {
            AllocTraits::construct(alloc, node, std::forward<Args>(args)...);
            insert_result = insert(std::move(*node));
        } catch (...) {
            AllocTraits::deallocate(alloc, node, 1);
            throw;
        }
        AllocTraits::deallocate(alloc, node, 1);
        return insert_result;
    }

    Value& operator[](const Key& key) {
        auto result = insert(std::pair<Key, Value>(key, Value()));
        return result.first->second;
    }

    Value& operator[](Key&& key) {
        auto result = insert(std::pair<Key, Value>(std::move(key), Value()));
        return result.first->second;
    }

    Value& at(const Key& key) {
        auto find_result = find_in_bucket(key);
        if (!find_result.first)
            throw std::out_of_range("No such key in UnorderedMap");
        return find_result.second->data.second;
    }

    const Value& at(const Key& key) const {
        auto find_result = find_in_bucket(key);
        if (!find_result.first)
            throw std::out_of_range("No such key in UnorderedMap");
        return find_result.second->data.second;
    }

    void erase(const_iterator it) noexcept {
        size_t it_hash = it.hash();
        if (static_cast<const_iterator>(table[it_hash % table.size()]) == it) {
            auto place = table[it_hash % table.size()];
            ++place;
            if (static_cast<const_iterator>(place) != end() && place->hash == it_hash)
                table[it_hash % table.size()] = place;
            else
                table[it_hash % table.size()] = place;
        }
        list.erase(it.getListIterator());
    }

    void erase(const_iterator first, const_iterator last) noexcept {
        for (const_iterator it = first; it != last;) {
            auto cp = it;
            it++;
            erase(cp);
        }
    }

    iterator find(const Key& key) noexcept {
        auto find_result = find_in_bucket(key);
        if (!find_result.first)
            return end();
        return find_result.second;
    }

    float load_factor() const noexcept {
        return static_cast<float>(size()) / static_cast<float>(table.size());
    }

    float max_load_factor() const noexcept {
        return m_max_load_factor;
    }

    void max_load_factor(float new_max_load) noexcept {
        m_max_load_factor = new_max_load;
        reserve(size());
    }

    void swap(UnorderedMap& other) {
        table.swap(other.table);
        list.swap(other.list);
        std::swap(m_max_load_factor, other.m_max_load_factor);
        std::swap(hash, other.hash);
        std::swap(equal, other.equal);
    }
};
