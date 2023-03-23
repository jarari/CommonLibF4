#pragma once

namespace RE {
	class BSAnimationGraphManager;
	class hkbAnimationBindingWithTriggers;
	class hkbClipGenerator;
	class hkbContext;

	class __declspec(novtable) IAnimationClipLoaderSingleton {
	public:
		static constexpr auto RTTI{ RTTI::IAnimationClipLoaderSingleton };
		static constexpr auto VTABLE{ VTABLE::IAnimationClipLoaderSingleton };

		virtual ~IAnimationClipLoaderSingleton();

		virtual hkbAnimationBindingWithTriggers* GetBindingForClipGeneratorImpl(hkbContext const&, hkbClipGenerator&) = 0;
		virtual hkbAnimationBindingWithTriggers* GetBindingForDynamicClipImpl(hkbContext const&, hkbClipGenerator&) = 0;
		virtual void DeactivateDynamicClipImpl(hkbContext const&, hkbClipGenerator&) = 0;
		virtual hkBool CheckLoadingClipImpl(hkbContext const&, hkbClipGenerator&) = 0;
	};

	class __declspec(novtable) AnimationFileManagerSingleton : public IAnimationClipLoaderSingleton, public BSTSingletonSDM<AnimationFileManagerSingleton> {
	public:
		static constexpr auto RTTI{ RTTI::AnimationFileManagerSingleton };
		static constexpr auto VTABLE{ VTABLE::AnimationFileManagerSingleton };

		static AnimationFileManagerSingleton* GetSingleton() {
			REL::Relocation<AnimationFileManagerSingleton**> singleton{ REL::ID(908588) };
			return *singleton;
		}

		bool RequestIdles(BSFixedString const& animEvent, BSTSmartPointer<BShkbAnimationGraph> const& animGraph, BSFixedString const& path, BSTSmartPointer<BSAnimationGraphManager> const& animGraphManager) {
			using func_t = decltype(&AnimationFileManagerSingleton::RequestIdles);
			REL::Relocation<func_t> func{ REL::ID(261705) };
			return func(this, animEvent, animGraph, path, animGraphManager);
		}

		bool IsLoading(BSFixedString const& path, BSTSmartPointer<BSAnimationGraphManager> const& animGraphManager) {
			using func_t = decltype(&AnimationFileManagerSingleton::IsLoading);
			REL::Relocation<func_t> func{ REL::ID(393102) };
			return func(this, path, animGraphManager);
		}

		void Update() {
			using func_t = decltype(&AnimationFileManagerSingleton::Update);
			REL::Relocation<func_t> func{ REL::ID(286088) };
			return func(this);
		}
	};
}
