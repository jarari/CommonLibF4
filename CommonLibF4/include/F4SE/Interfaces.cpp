#include "F4SE/Interfaces.h"

#include "F4SE/API.h"
#include "F4SE/Logger.h"

namespace F4SE
{
	bool MessagingInterface::RegisterListener(stl::zstring a_sender, EventCallback* a_handler) const
	{
		const auto success =
			GetProxy().RegisterListener(
				GetPluginHandle(),
				a_sender.data(),
				a_handler);
		if (!success) {
			log::warn("failed to register listener for {}", a_sender);
		}
		return success;
	}

	bool MessagingInterface::Dispatch(std::uint32_t a_messageType, void* a_data, std::uint32_t a_dataLen, const char* a_receiver) const
	{
		const auto success =
			GetProxy().Dispatch(
				GetPluginHandle(),
				a_messageType,
				a_data,
				a_dataLen,
				a_receiver);
		if (!success) {
			log::warn("failed to dispatch to {}", (a_receiver ? a_receiver : "all listeners"));
		}
		return success;
	}

	bool ScaleformInterface::Register(stl::zstring a_name, RegisterCallback* a_callback) const
	{
		const auto success =
			GetProxy().Register(
				a_name.data(),
				a_callback);
		if (!success) {
			log::warn("failed to register {}", a_name);
		}
		return success;
	}

	void SerializationInterface::SetUniqueID(std::uint32_t a_uid)
	{
		GetProxy().SetUniqueID(
			GetPluginHandle(),
			a_uid);
	}

	void SerializationInterface::SetRevertCallback(EventCallback* a_callback) const
	{
		GetProxy().SetRevertCallback(
			GetPluginHandle(),
			a_callback);
	}

	void SerializationInterface::SetSaveCallback(EventCallback* a_callback) const
	{
		GetProxy().SetSaveCallback(
			GetPluginHandle(),
			a_callback);
	}

	void SerializationInterface::SetLoadCallback(EventCallback* a_callback) const
	{
		GetProxy().SetLoadCallback(
			GetPluginHandle(),
			a_callback);
	}

	void SerializationInterface::SetFormDeleteCallback(FormDeleteCallback* a_callback) const
	{
		GetProxy().SetFormDeleteCallback(
			GetPluginHandle(),
			a_callback);
	}

	bool SerializationInterface::WriteRecord(std::uint32_t a_type, std::uint32_t a_version, const void* a_buf, std::uint32_t a_length) const
	{
		const auto success =
			GetProxy().WriteRecord(
				a_type,
				a_version,
				a_buf,
				a_length);
		if (!success) {
			log::warn("failed to write record");
		}
		return success;
	}

	bool SerializationInterface::OpenRecord(std::uint32_t a_type, std::uint32_t a_version) const
	{
		const auto success =
			GetProxy().OpenRecord(
				a_type,
				a_version);
		if (!success) {
			log::warn("failed to open record");
		}
		return success;
	}

	bool SerializationInterface::WriteRecordData(const void* a_buf, std::uint32_t a_length) const
	{
		const auto success =
			GetProxy().WriteRecordData(
				a_buf,
				a_length);
		if (!success) {
			log::warn("failed to write record data");
		}
		return success;
	}

	bool SerializationInterface::GetNextRecordInfo(std::uint32_t& a_type, std::uint32_t& a_version, std::uint32_t& a_length) const
	{
		const auto success =
			GetProxy().GetNextRecordInfo(
				std::addressof(a_type),
				std::addressof(a_version),
				std::addressof(a_length));
		if (!success) {
			log::warn("failed to get next record info");
		}
		return success;
	}

	std::uint32_t SerializationInterface::ReadRecordData(void* a_buf, std::uint32_t a_length) const
	{
		const auto read = GetProxy().ReadRecordData(
			a_buf,
			a_length);
		if (read != a_length) {
			log::warn("failed to read full record data {}B of {}B", read, a_length);
		}
		return read;
	}

	bool PapyrusInterface::Register(RegisterFunctions* a_callback) const
	{
		const auto success = GetProxy().Register(a_callback);
		if (!success) {
			log::warn("failed to register callback");
		}
		return success;
	}
}
