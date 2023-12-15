#pragma once

#include "RE/Bethesda/Actor.h"
#include "RE/Bethesda/BSFixedString.h"
#include "RE/Bethesda/TESBoundObjects.h"
#include "RE/Bethesda/bhkPickData.h"
#include "RE/NetImmerse/NiAVObject.h"
#include "RE/NetImmerse/NiPoint3.h"

namespace RE
{
	namespace CombatUtilities
	{
		inline bool CalculateProjectileLOS(
			Actor* a_actor,
			BGSProjectile* a_projectile,
			float a_projectileSpeed,
			const NiPoint3& a_launchPos,
			const NiPoint3& a_targetPos,
			NiPoint3* a_hitPos,
			TESObjectREFR** a_collidee,
			float* a_distanceFraction)
		{
			using func_t = bool (*)(Actor*, BGSProjectile*, float, const NiPoint3&, const NiPoint3&, NiPoint3*, TESObjectREFR**, float*);
			REL::Relocation<func_t> func{ REL::ID(798616) };
			return func(a_actor, a_projectile, a_projectileSpeed, a_launchPos, a_targetPos, a_hitPos, a_collidee, a_distanceFraction);
		}

		inline bool CalculateProjectileLOS(Actor* a_actor, BGSProjectile* a_projectile, bhkPickData& a_pickData)
		{
			using func_t = bool (*)(Actor*, BGSProjectile*, bhkPickData&);
			REL::Relocation<func_t> func{ REL::ID(55339) };
			return func(a_actor, a_projectile, a_pickData);
		}

		inline bool CalculateProjectileTrajectory(
			const NiPoint3& a_projectilePos,
			const NiPoint3& a_projectileVelocity,
			float a_projectileGravity,
			const NiPoint3& a_targetPos,
			float a_heading,
			NiPoint3& a_trajectoryPos)
		{
			using func_t = decltype(&CalculateProjectileTrajectory);
			REL::Relocation<func_t> func{ REL::ID(1575156) };
			return func(a_projectilePos, a_projectileVelocity, a_projectileGravity, a_targetPos, a_heading, a_trajectoryPos);
		}

		inline bool CalculateProjectileLOS(Actor* a, BGSProjectile* proj, float speed, const NiPoint3& launchPos, const NiPoint3& targetPos, NiPoint3* hitPos, TESObjectREFR** collidee, float* dist)
		{
			typedef bool func_t(Actor*, BGSProjectile*, float, const NiPoint3&, const NiPoint3&, NiPoint3*, TESObjectREFR**, float*);
			REL::Relocation<func_t> func{ REL::ID(798616) };
			return func(a, proj, speed, launchPos, targetPos, hitPos, collidee, dist);
		}

		inline bool CalculateProjectileLOS(Actor* a, BGSProjectile* proj, bhkPickData& pick)
		{
			typedef bool func_t(Actor*, BGSProjectile*, bhkPickData&);
			REL::Relocation<func_t> func{ REL::ID(55339) };
			return func(a, proj, pick);
		}

		inline bool CheckLOS(Actor& a_actor, Actor& a_target)
		{
			typedef bool func_t(Actor&, Actor&);
			REL::Relocation<func_t> func{ REL::ID(1403705) };
			return func(a_actor, a_target);
		}

		static REL::Relocation<float> fWorldGravity{ REL::ID(1378547) };
	};

	namespace AnimationSystemUtils
	{
		inline bool WillEventChangeState(const TESObjectREFR& a_ref, const BSFixedString& a_evn)
		{
			using func_t = decltype(&WillEventChangeState);
			REL::Relocation<func_t> func{ REL::ID(35074) };
			return func(a_ref, a_evn);
		}
	}

	namespace BGSAnimationSystemUtils
	{
		struct ActiveSyncInfo
		{
		public:
			// members
			BSTObjectArena<BSTTuple<BSFixedString, float>> otherSyncInfo;  // 00
			float currentAnimTime;                                         // 40
			float animSpeedMult;                                           // 44
			float totalAnimTime;                                           // 48
		};
		static_assert(sizeof(ActiveSyncInfo) == 0x50);

		inline bool GetActiveSyncInfo(const IAnimationGraphManagerHolder* a_graphHolder, ActiveSyncInfo& a_infoOut)
		{
			using func_t = decltype(&GetActiveSyncInfo);
			REL::Relocation<func_t> func{ REL::ID(1349978) };
			return func(a_graphHolder, a_infoOut);
		}

		inline bool InitializeActorInstant(Actor& a_actor, bool a_update3D)
		{
			using func_t = decltype(&InitializeActorInstant);
			REL::Relocation<func_t> func{ REL::ID(672857) };
			return func(a_actor, a_update3D);
		}

		inline bool IsActiveGraphInTransition(const TESObjectREFR* a_refr)
		{
			using func_t = decltype(&IsActiveGraphInTransition);
			REL::Relocation<func_t> func{ REL::ID(839650) };
			return func(a_refr);
		}
	};

	namespace PerkUtilities
	{
		inline void RemoveGrenadeTrajectory()
		{
			using func_t = decltype(&RemoveGrenadeTrajectory);
			REL::Relocation<func_t> func{ REL::ID(672186) };
			return func();
		}
	}
}
