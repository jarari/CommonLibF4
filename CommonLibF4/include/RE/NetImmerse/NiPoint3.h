#pragma once

namespace RE
{
	class NiPoint3
	{
	public:
		using value_type = float;
		using size_type = std::size_t;
		using reference = value_type&;
		using const_reference = const value_type&;
		using pointer = value_type*;
		using const_pointer = const value_type*;

		[[nodiscard]] reference operator[](size_type a_pos) noexcept
		{
			assert(a_pos < 3);
			return reinterpret_cast<pointer>(std::addressof(x))[a_pos];
		}

		[[nodiscard]] const_reference operator[](size_type a_pos) const noexcept
		{
			assert(a_pos < 3);
			return reinterpret_cast<const_pointer>(std::addressof(x))[a_pos];
		}

		[[nodiscard]] bool operator==(const NiPoint3& a_rhs) const
		{
			return (x == a_rhs.x && y == a_rhs.y && z == a_rhs.z);
		}

		[[nodiscard]] NiPoint3 operator+(const NiPoint3& pt) const
		{
			return NiPoint3(x + pt.x, y + pt.y, z + pt.z);
		}

		[[nodiscard]] NiPoint3 operator-(const NiPoint3& pt) const
		{
			return NiPoint3(x - pt.x, y - pt.y, z - pt.z);
		}

		[[nodiscard]] NiPoint3 operator*(float s) const
		{
			return NiPoint3(x * s, y * s, z * s);
		}

		[[nodiscard]] NiPoint3 operator/(float s) const
		{
			return NiPoint3(x / s, y / s, z / s);
		}

		[[nodiscard]] float GetZAngleFromVector()
		{
			using func_t = decltype(&NiPoint3::GetZAngleFromVector);
			REL::Relocation<func_t> func{ REL::ID(1450064) };
			return func(this);
		}

		// members
		value_type x{ 0.0F };  // 0
		value_type y{ 0.0F };  // 4
		value_type z{ 0.0F };  // 8
	};
	static_assert(sizeof(NiPoint3) == 0xC);

	class alignas(0x10) NiPoint3A :
		public NiPoint3
	{
	public:
	};
	static_assert(sizeof(NiPoint3A) == 0x10);
}
