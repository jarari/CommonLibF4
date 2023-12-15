#pragma once

#include <RE/Bethesda/Actor.h>
#include <RE/Bethesda/BSFixedString.h>
#include <RE/Bethesda/TESBoundObjects.h>
#include <RE/Bethesda/bhkPickData.h>
#include <RE/NetImmerse/NiAVObject.h>
#include <RE/NetImmerse/NiPoint3.h>
#include <REL/Relocation.h>

namespace RE
{
	namespace BSUtilities
	{
		inline NiAVObject* GetObjectByName(NiAVObject* root, const BSFixedString& name, bool tryInternal, bool dontAttach)
		{
			using func_t = decltype(&GetObjectByName);
			REL::Relocation<func_t> func{ REL::ID(843650) };
			return func(root, name, tryInternal, dontAttach);
		}
	}

	namespace CombatUtilities
	{
		inline bool CalculateProjectileTrajectory(const NiPoint3& pos, const NiPoint3& vel, float gravity, const NiPoint3& targetPos, float X, NiPoint3& out)
		{
			using func_t = decltype(&CalculateProjectileTrajectory);
			REL::Relocation<func_t> func{ REL::ID(1575156) };
			return func(pos, vel, gravity, targetPos, X, out);
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
		inline bool WillEventChangeState(const TESObjectREFR& ref, const BSFixedString& evn)
		{
			using func_t = decltype(&WillEventChangeState);
			REL::Relocation<func_t> func{ REL::ID(35074) };
			return func(ref, evn);
		}
	}

	namespace BGSAnimationSystemUtils
	{
		inline bool InitializeActorInstant(Actor& a, bool b)
		{
			using func_t = decltype(&InitializeActorInstant);
			REL::Relocation<func_t> func{ REL::ID(672857) };
			return func(a, b);
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
