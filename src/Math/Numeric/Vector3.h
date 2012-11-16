#pragma once

#include <cmath>
#include <string>
#include <sstream>

namespace Math { namespace Numeric {

	class Vector3
	{
	public:
		double x;
		double y;
		double z;

	public:
		Vector3(double _x = 0.0, double _y = 0.0, double _z = 0.0)
			: x(_x)
			, y(_y)
			, z(_z)
		{
		}

		double abs() const
		{
			return sqrt(x * x + y * y + z * z);
		}

		double abs2() const
		{
			return x * x + y * y + z * z;
		}

		Vector3 normalized() const
		{
			return *this / abs();
		}

		Vector3 operator+() const
		{
			return *this;
		}

		Vector3 operator-() const
		{
			return Vector3(-x, -y, -z);
		}

		Vector3 operator+(const Vector3 & that) const
		{
			return Vector3(x + that.x, y + that.y, z + that.z);
		}

		Vector3 operator-(const Vector3 & that) const
		{
			return Vector3(x - that.x, y - that.y, z - that.z);
		}

		Vector3 operator*(double mul) const
		{
			return Vector3(x * mul, y * mul, z * mul);
		}

		Vector3 operator/(double div) const
		{
			return *this * (1.0 / div);
		}

		double operator*(const Vector3 & that) const
		{
			return x * that.x + y * that.y + z * that.z;
		}

		const Vector3 operator^(const Vector3 & that) const
		{
			return Vector3(
				y * that.z - z * that.y,
				z * that.x - x * that.z,
				x * that.y - y * that.x);
		}

		Vector3 & operator+=(const Vector3 & that)
		{
			x += that.x;
			y += that.y;
			z += that.z;
			return *this;
		}

		Vector3 & operator-=(const Vector3 & that)
		{
			x -= that.x;
			y -= that.y;
			z -= that.z;
			return *this;
		}

		std::string toString() const
		{
			std::ostringstream out;
			out << "(" << x << "; " << y << "; " << z << ")";
			return out.str();
		}
	};

}}
