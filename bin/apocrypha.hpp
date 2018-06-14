#ifndef NETWORK_HPP
#define NETWORK_HPP
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#include <iostream>
#include <vector>
#define h_addr h_addr_list[0]  
#define check(condition, message) \
    if (condition) { perror(message); exit(1); }
using query = std::vector<std::string>;
namespace Network {
    void write(const int, const query);
    void write(const int, const std::string);
    std::string read(const int);
    int create_socket(const char *, const int);
};
#endif
#ifndef CLIENT_H
#define CLIENT_H
#include "json.hpp"
using json = nlohmann::json;
using strings = std::vector<std::string>;
class Client {
    public:
        Client(void) {
            socket = Network::create_socket(
                    default_host, default_port);
        }
        Client(const char *host, const int port = 9999) {
            socket = Network::create_socket(host, port);
        }
        ~Client() {
            close(socket);
        }
        json get(const query = {});
        strings keys(const query = {});
        void set(const query, const json);
        void append(const query, const std::string);
        void append(const query, const strings);
        void remove(const query, const std::string);
        void remove(const query, const strings);
        void del(const query);
    private:
        int socket = 0;
        const int default_port = 9999;
        const char *default_host = "localhost";
        void write(const query q) {
            Network::write(socket, q);
        }
        std::string read(void) {
            return Network::read(socket);
        }
};
#endif
json Client::get(const query q) {
    query query = q;
    query.push_back("--edit");
    write(query);
    return json::parse(read());
}
strings Client::keys(const query q) {
    std::vector<std::string> result;
    query query = q;
    query.push_back("--keys");
    write(query);
    auto response = read();
    std::string item = "";
    for (auto it = response.begin(); it != response.end(); ++it) {
        if ((*it) == '\n') {
            result.push_back(item);
            item = "";
        } else {
            item += (*it);
        }
    }
    return result;
}
void Client::set(const query q, const json value) {
    query query = q;
    query.push_back("--set");
    query.push_back( value.dump() );
    write(query);
    (void)read();
}
void Client::append(const query q, std::string value) {
    strings s{value};
    append(q, s);
}
void Client::append(const query q, strings values) {
    query query = q;
    query.push_back("+");
    for (const auto& item : values) {
        query.push_back(item);
    }
    write(query);
    (void)read();
}
void Client::remove(const query q, std::string value) {
    strings s{value};
    remove(q, s);
}
void Client::remove(const query q, strings values) {
    query query = q;
    query.push_back("-");
    for (const auto& item : values) {
        query.push_back(item);
    }
    write(query);
    (void)read();
}
void Client::del(const query q) {
    query query = q;
    query.push_back("--del");
    write(query);
    (void)read();
}
void Network::write(const int socket, const std::string query) {
    std::string message = query;
    int written;
    int length = htonl(message.size());
    written = ::write(socket, &length, sizeof(length));
    check(written < 0, "error writing to socket");
    written = ::write(socket, message.c_str(), message.size());
    check(written < 0, "error writing to socket");
}
void Network::write(const int socket, const query query) {
    std::string prepared_query;
    for (auto& e : query) {
        prepared_query += e + "\n";
    }
    write(socket, prepared_query);
}
std::string Network::read(const int socket) {
    std::string result = "";
    int length;
    ::read(socket, &length, sizeof(int));
    length = ntohl(length);
    int total_bytes = 0;
    const int buffer_size = 4096;
    char buffer[buffer_size];
    do {
        memset(buffer, 0, buffer_size);
        int bytes_read = ::read(
                socket, buffer, length - total_bytes);
        if (bytes_read < 0) {
            return nullptr;
        }
        total_bytes += bytes_read;
        result += buffer;
    } while (total_bytes < length);
    return result;
}
int Network::create_socket(const char *host, const int port) {
    int sockfd = ::socket(AF_INET, SOCK_STREAM, 0);
    check(sockfd < 0, "error opening socket");
    struct sockaddr_in serv_addr;
    memset(&serv_addr, 0, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    struct hostent *server = gethostbyname(host);
    memcpy(
            server->h_addr,
            &serv_addr.sin_addr.s_addr,
            server->h_length);
    serv_addr.sin_port = htons(port);
    check(
            connect(
                sockfd,
                (struct sockaddr *)&serv_addr,
                sizeof(serv_addr)
                ) < 0,
            "error connecting to remote host");
    return sockfd;
}
