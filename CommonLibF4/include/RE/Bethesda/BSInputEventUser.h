#pragma once

#include "RE/Bethesda/InputEvent.h"
#include "RE/Bethesda/MemoryManager.h"

namespace RE
{
	class alignas(0x08) BSInputEventUser
	{
	public:
		static constexpr auto RTTI{ RTTI::BSInputEventUser };
		static constexpr auto VTABLE{ VTABLE::BSInputEventUser };

		virtual ~BSInputEventUser() = default;  // 00

		// add
		virtual bool ShouldHandleEvent(const InputEvent*) { return false; }  // 01
		virtual void OnKinectEvent(const KinectEvent*) { return; }
		virtual void OnDeviceConnectEvent(const DeviceConnectEvent*) { return; }
		virtual void OnThumbstickEvent(const ThumbstickEvent*) { return; }
		virtual void OnCursorMoveEvent(const CursorMoveEvent*) { return; }
		virtual void OnMouseMoveEvent(const MouseMoveEvent*) { return; }
		virtual void OnCharacterEvent(const CharacterEvent*) { return; }
		virtual void OnButtonEvent(const ButtonEvent*) { return; }

		F4_HEAP_REDEFINE_NEW(BSInputEventUser);

		// members
		bool inputEventHandlingEnabled{ true };  // 08
	};
	static_assert(sizeof(BSInputEventUser) == 0x10);

	struct __declspec(novtable) DisconnectHandler :
		public BSInputEventUser
	{
	public:
		static constexpr auto RTTI{ RTTI::DisconnectHandler };
		static constexpr auto VTABLE{ VTABLE::DisconnectHandler };
	};
	static_assert(sizeof(DisconnectHandler) == 0x10);
}
