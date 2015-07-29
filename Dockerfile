FROM ubuntu:14.04
MAINTAINER Tim Sell <tim@saltcog.com>

RUN DEBIAN_FRONTEND=noninteractive apt-get update --fix-missing && apt-get install -y build-essential python-setuptools git python
RUN easy_install pip

WORKDIR /opt/code/

# Add requirements and install
ADD . /opt/code/
RUN pip install -r ./requirements.txt

# expose port(s)
EXPOSE 8000

CMD python -m SimpleHTTPServer 8000