#pragma once

#include <cmath>
#include <string>
#include <sstream>

namespace Math { namespace Numeric {

	class Vector2
	{
	public:
		double x;
		double y;

	public:
		Vector2(double _x = 0.0, double _y = 0.0)
			: x(_x)
			, y(_y)
		{
		}

		double length() const
		{
			return sqrt(x * x + y * y);
		}

		double length2() const
		{
			return x * x + y * y;
		}

		double atan2() const
		{
			return atan2l(y, x);
		}

		const Vector2 normalized() const
		{
			return *this / length();
		}

		const Vector2 ort() const
		{
			return Vector2(-y, x);
		}

		const Vector2 operator+() const
		{
			return *this;
		}

		const Vector2 operator-() const
		{
			return Vector2(-x, -y);
		}

		const Vector2 operator+(const Vector2 & that) const
		{
			return Vector2(x + that.x, y + that.y);
		}

		const Vector2 operator-(const Vector2 & that) const
		{
			return Vector2(x - that.x, y - that.y);
		}

		const Vector2 operator*(double mul) const
		{
			return Vector2(x * mul, y * mul);
		}

		const Vector2 operator/(double div) const
		{
			return *this * (1.0 / div);
		}

		double operator*(const Vector2 & that) const
		{
			return x * that.x + y * that.y;
		}

		double operator^(const Vector2 & that) const
		{
			return x * that.y - y * that.x;
		}

		const Vector2 & operator+=(const Vector2 & that)
		{
			x += that.x, y += that.y;
			return *this;
		}

		const Vector2 & operator-=(const Vector2 & that)
		{
			x -= that.x, y -= that.y;
			return *this;
		}

		std::string toString() const
		{
			std::ostringstream out;
			out << "(" << x << "; " << y << ")";
			return out.str();
		}

		double distToSegment(const Vector2 & a, Vector2 b) const
		{
			b -= a;
			Vector2 c = *this - a;
			if(b * c <= 0)
				return c.length();
			else if(b * c >= b * b)
				return (c - b).length();
			else
				return fabs((b ^ c) / b.length());
		}

	public:
		static const Vector2 polar(double phi)
		{
			return Vector2(cos(phi), sin(phi));
		}

		static const Vector2 polar(double r, double phi)
		{
			return Vector2(r * cos(phi), r * sin(phi));
		}

		static double distance(const Vector2 & a, const Vector2 & b)
		{
			const double dx = a.x - b.x;
			const double dy = a.y - b.y;
			return sqrt(dx * dx + dy * dy);
		}

		static const Vector2 getCircleCenter(const Vector2 & p1, const Vector2 & p2, const Vector2 & p3)
		{
			double a1 = 2.0 * (p3.x - p1.x);
			double b1 = 2.0 * (p3.y - p1.y);
			double c1 = p3.length2() - p1.length2();

			double a2 = 2.0 * (p3.x - p2.x);
			double b2 = 2.0 * (p3.y - p2.y);
			double c2 = p3.length2() - p2.length2();

			double det = a1 * b2 - a2 * b1;
			return Vector2((c1 * b2 - c2 * b1) / det, (a1 * c2 - a2 * c1) / det);
		}

		static const Vector2 intersection(const Vector2 & a1, const Vector2 & b1, const Vector2 & a2, const Vector2 & b2)
		{
			const double a11 = (b1 - a1).y, a12 = -(b1 - a1).x;
			const double a21 = (b2 - a2).y, a22 = -(b2 - a2).x;
			const double d1 = a1 ^ b1, d2 = a2 ^ b2;
			const double det = a11 * a22 - a12 * a21;
			return Vector2((d1 * a22 - d2 * a12) / det, (a11 * d2 - a21 * d1) / det);
		}
	};

}}
