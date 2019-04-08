#include <algorithm>
#include <iostream>
#include <map>
#include <vector>
#include <utility>

template <typename T>
class Polynomial {
private:
    std::map<size_t, T> coef;

    void Delete_Zeros() {
        auto it = coef.begin();
        while (it != coef.end()) {
            if ((*it).second != 0)
                ++it;
            else
                coef.erase(it++);
        }
    }
public:
    Polynomial(const std::vector<T>& v) {
        size_t deg = 0;
        for (auto elem : v)
            coef[deg++] = elem;
        Delete_Zeros();
    }

    Polynomial(T c = T()) {
        if (c != T())
            coef[0] = c;
    }

    template <typename Iter>
    Polynomial(Iter first, Iter last) {
        size_t deg = 0;
        while (first != last)
            coef[deg++] = *first++;
        Delete_Zeros();
    }

    int Degree() const {  // returns degree of polynomial or -1 if it is zero polynomial
        if (!coef.empty())
            return (*coef.rbegin()).first;
        return -1;
    }

    T operator[] (size_t degree) const {  // returns coefficient before this degree
        if (coef.find(degree) != coef.end())
            return coef.at(degree);
        else
            return T();  // returns default value if requested degree is higher than max degree
    }

    bool operator == (const Polynomial<T>& other) const {
        return coef == other.coef;
    }

    bool operator != (const Polynomial<T>& other) const {
        return coef != other.coef;
    }

    Polynomial<T> operator + (const Polynomial<T>& other) const {  // sum of two polynomials
        Polynomial<T> res{*this};
        res += other;
        return res;
    }

    Polynomial<T> operator - (const Polynomial<T>& other) const {  // difference of two polynomials
        Polynomial<T> res{*this};
        res -= other;
        return res;
    }

    Polynomial<T> operator * (const Polynomial<T>& other) const {  // product of two polynomials
        Polynomial<T> res;
        for (int i = 0; i <= (*this).Degree(); ++i) {
            for (int j = 0; j <= other.Degree(); ++j)
                res.coef[i + j] += (*this)[i] * other[j];
        }
        res.Delete_Zeros();
        return res;
    }

    Polynomial<T>& operator += (const Polynomial<T>& other) {
        for (int i = 0; i <= other.Degree(); ++i)
            coef[i] += other[i];
        Delete_Zeros();
        return *this;
    }

    Polynomial<T>& operator -= (const Polynomial<T>& other) {
        for (int i = 0; i <= other.Degree(); ++i)
            coef[i] -= other[i];
        Delete_Zeros();
        return *this;
    }

    Polynomial<T>& operator *= (const Polynomial<T>& other) {
        Polynomial<T> poly = *this * other;
        *this = poly;
        return *this;
    }

    T operator() (T value) const {  // calculates f(value)
        T ans = T();
        for (int i = (*this).Degree(); i >= 0; --i) {
            ans = (*this)[i] + ans * value;  // Horner's method
        }
        return ans;
    }

    typename std::map<size_t, T>::const_iterator begin() const {
        return coef.begin();
    }

    typename std::map<size_t, T>::const_iterator end() const {
        return coef.end();
    }

    Polynomial<T> operator & (const Polynomial<T>& other) const {  // returns composition
        Polynomial<T> composition;
        for (int i = 0; i <= (*this).Degree(); ++i) {
            if ((*this)[i] != T()) {
                Polynomial<T> tmp{T((*this)[i])};
                for (int j = 1; j != i + 1; ++j)
                    tmp *= other;
                composition += tmp;
            }
        }
        return composition;
    }

    Polynomial<T> operator / (const Polynomial<T>& other) const {
        Polynomial<T> first = *this;
        std::vector<T> result((*this).Degree() + 1);
        int fd = first.Degree();
        while (fd >= other.Degree()) {
            T t = first[fd] / other[other.Degree()];
            std::vector<T> tmp;
            for (int i = 0; i != fd - other.Degree(); ++i)
                tmp.push_back(T());
            tmp.push_back(t);
            Polynomial<T> p {tmp};
            first -= other * p;
            result[fd - other.Degree()] += t;
            fd = first.Degree();
        }
        return Polynomial<T> {result};
    }

    Polynomial<T> operator % (const Polynomial<T>& other) const {  // returns remainder
        return *this - (*this / other) * other;
    }

    Polynomial<T> operator , (const Polynomial<T>& other) const {  // returns PolynomialGCD
        Polynomial<T> first = *this, second = other;
        if (first.Degree() < second.Degree()) {
            std::swap(first, second);
        }
        while (second.Degree() > 0) {
            Polynomial<T> tmp = second;
            first = first % second;
            second = first;
            first = tmp;
        }
        if (second != T(0))
            return Polynomial(T(1));
        first = first / first[first.Degree()];
        return first;
    }
};

template <typename T>
std::ostream& operator << (std::ostream& out, const Polynomial<T>& pol) {
    int deg = pol.Degree();
    if (deg == -1) {
        out << '0';  // zero polynomial
    } else {
        for (int i = deg; i != -1; --i) {
            if (pol[i] != T(0)) {  // printing only degree with non-zero coefficient
                if (pol[i] != T(1) && pol[i] != T(-1)) {
                    if (i != deg && pol[i] > T(0)) {
                        out << "+";
                    }
                    out << pol[i];
                    if (i > 1)
                        out << "*x^" << i;
                    if (i == 1)
                        out << "*x";
                } else if (pol[i] == T(-1)) {  // coefficient -1 for non-zero degree prints as -
                    if (i > 1)
                        out << "-x^" << i;
                    if (i == 1)
                        out << "-x";
                    if (i == 0)
                        out << pol[i];
                } else if (pol[i] == T(1)) {
                    if (i != deg)
                        out << "+";
                    if (i > 1)
                        out << "x^" << i;
                    if (i == 1)
                        out << "x";
                    if (i == 0)
                        out << pol[i];
                }
            }
        }
    }
    return out;
}
