#pragma once

#include <memory>


template<size_t N>
class StackStorage {
private:
    char buffer[N]{};
    size_t used = 0;

public:
    StackStorage() = default;

    StackStorage(const StackStorage&) = delete;

    StackStorage& operator=(const StackStorage&) = delete;

    ~StackStorage() = default;

    char* allocateMemory(size_t type_size, size_t count) {
        if (used + (type_size - used % type_size) % type_size + count * type_size > N)
            throw std::bad_alloc();
        used += (type_size - used % type_size) % type_size;
        used += count * type_size;
        return &buffer[used - count * type_size];
    }
};


template<typename T, size_t N>
class StackAllocator {
public:
    StackStorage<N>* stack_storage;

    using value_type = T;

    StackAllocator() = delete;

    StackAllocator(StackStorage<N>& storage) : stack_storage(&storage) {}

    template<typename U>
    StackAllocator(StackAllocator<U, N> other): stack_storage(other.stack_storage) {}

    T* allocate(size_t count) {
        return reinterpret_cast<T*>(stack_storage->allocateMemory(sizeof(T), count));
    }

    void deallocate(T*, size_t) {}

    template<typename U>
    //Clang-tidy want this name to be "Rebind", but it won't work
    //NOLINTNEXTLINE
    struct rebind {
        using other = StackAllocator<U, N>;
    };
};


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
