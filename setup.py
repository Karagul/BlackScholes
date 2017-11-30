import os
from setuptools import setup, find_packages


    def read(fname):
        try:
            return open(os.path.join(os.path.dirname(__file__), fname)).read()
        except:
            return 'Please see: https://github.com//BlackScholes'

    setup(
        name='BlackScholes',
        version='0.0.1',
        author='Tyler Roberts',
        author_email='',
        description='',
        long_description=read('README.md'),
        license='',
        keywords='',
        # download_url='',
        packages=find_packages(),
        install_requires=['scipy','numpy'],
        classifiers=[],
        include_package_data=True
    )
