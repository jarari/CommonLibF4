#pragma once

#include "RE/Havok/hkArray.h"
#include "RE/Havok/hknpCollisionQueryCollector.h"
#include "RE/Havok/hknpCollisionResult.h"

namespace RE {
	class __declspec(novtable) hknpClosestHitCollector :
		public hknpCollisionQueryCollector  // 000
	{
	public:
		static constexpr auto RTTI{ RTTI::hknpClosestHitCollector };
		static constexpr auto VTABLE{ VTABLE::hknpClosestHitCollector };

		hknpClosestHitCollector() {
			typedef hknpClosestHitCollector* func_t(hknpClosestHitCollector*);
			REL::Relocation<func_t> func{ REL::ID(951692), 0x10 };
			func(this);
		}

		// override (hknpCollisionQueryCollector)
		void Reset() override;                                // 01
		void AddHit(const hknpCollisionResult&) override;     // 02
		bool HasHit() const override;                         // 03
		std::int32_t GetNumHits() const override;             // 04
		const hknpCollisionResult* GetHits() const override;  // 05

		// members
		hknpCollisionResult result;							//0x20
		bool hasHit;										//0x80
		int8_t pad[15];
	};
	static_assert(sizeof(hknpClosestHitCollector) == 0x90);
}
